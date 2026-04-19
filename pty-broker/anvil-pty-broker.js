#!/usr/bin/env node
// anvil-pty-broker — node-pty TCP broker (Phase 1 MVP).
// Design: docs/design/04-pty-broker.org.
//
// Usage:
//   node anvil-pty-broker.js --port 7641 --token TOKEN [--allow CMD]...
//
// The broker listens on 127.0.0.1:PORT, accepts JSON-line frames, and
// responds with JSON-line events.  Output bytes are base64-encoded so
// the wire stays ASCII-safe.  First message from any client MUST be
//   {"op":"auth","token":"..."}
// or the connection is closed.  After auth, clients may:
//   {"op":"spawn", "id":ID, "cmd":CMD, "args":[...], "cwd":DIR, "cols":C, "rows":R}
//   {"op":"input", "id":ID, "data":TEXT, "b64":true?}   // b64 → decode before write
//   {"op":"kill",  "id":ID, "signal":"SIGTERM"?}
//   {"op":"list"}
// Events:
//   {"ev":"authed"}
//   {"ev":"spawned", "id":ID, "pid":N}
//   {"ev":"output",  "id":ID, "data":BASE64}
//   {"ev":"exit",    "id":ID, "code":N}
//   {"ev":"error",   "id":ID?, "message":M}
//   {"ev":"list",    "ids":[...]}

'use strict';

const net = require('net');

function parseArgs(argv) {
  const opts = { port: 7641, token: null, allow: [] };
  for (let i = 0; i < argv.length; i++) {
    const a = argv[i];
    if (a === '--port') opts.port = parseInt(argv[++i], 10);
    else if (a === '--token') opts.token = argv[++i];
    else if (a === '--allow') opts.allow.push(argv[++i]);
    else if (a === '--help' || a === '-h') {
      console.log('usage: anvil-pty-broker --port N --token T [--allow CMD]...');
      process.exit(0);
    }
  }
  if (!opts.token) {
    console.error('anvil-pty-broker: --token required');
    process.exit(2);
  }
  return opts;
}

function loadPty() {
  try {
    return require('node-pty');
  } catch (e) {
    console.error('anvil-pty-broker: node-pty not installed.  Run');
    console.error('  npm install --prefix ' + __dirname + ' node-pty');
    process.exit(3);
  }
}

function writeFrame(socket, obj) {
  try { socket.write(JSON.stringify(obj) + '\n'); } catch (_) { /* socket gone */ }
}

function start(opts) {
  const pty = loadPty();
  const ptys = new Map(); // id → { proc, socket }

  function cleanupSocket(socket) {
    for (const [id, entry] of ptys) {
      if (entry.socket === socket) {
        try { entry.proc.kill(); } catch (_) {}
        ptys.delete(id);
      }
    }
  }

  function handleSpawn(socket, msg) {
    if (!opts.allow.length) {
      writeFrame(socket, { ev: 'error', id: msg.id || null, message: 'spawn denied: allowlist empty' });
      return;
    }
    if (!opts.allow.includes(msg.cmd)) {
      writeFrame(socket, { ev: 'error', id: msg.id || null, message: 'spawn denied: ' + msg.cmd });
      return;
    }
    const id = String(msg.id || ('pty-' + Date.now()));
    if (ptys.has(id)) {
      writeFrame(socket, { ev: 'error', id, message: 'id in use' });
      return;
    }
    let proc;
    try {
      proc = pty.spawn(msg.cmd, Array.isArray(msg.args) ? msg.args : [], {
        name: 'xterm-256color',
        cols: msg.cols || 80,
        rows: msg.rows || 24,
        cwd: msg.cwd || process.cwd(),
        env: process.env,
      });
    } catch (e) {
      writeFrame(socket, { ev: 'error', id, message: String(e && e.message || e) });
      return;
    }
    ptys.set(id, { proc, socket });
    writeFrame(socket, { ev: 'spawned', id, pid: proc.pid });
    proc.onData((data) => {
      const b64 = Buffer.from(data, 'utf8').toString('base64');
      writeFrame(socket, { ev: 'output', id, data: b64 });
    });
    proc.onExit(({ exitCode, signal }) => {
      writeFrame(socket, { ev: 'exit', id, code: exitCode, signal: signal || null });
      ptys.delete(id);
    });
  }

  function handleInput(socket, msg) {
    const entry = ptys.get(String(msg.id));
    if (!entry) { writeFrame(socket, { ev: 'error', id: msg.id, message: 'unknown id' }); return; }
    let data = msg.data || '';
    if (msg.b64) {
      try { data = Buffer.from(data, 'base64').toString('utf8'); }
      catch (_) { writeFrame(socket, { ev: 'error', id: msg.id, message: 'bad b64' }); return; }
    }
    try { entry.proc.write(data); } catch (e) {
      writeFrame(socket, { ev: 'error', id: msg.id, message: String(e && e.message || e) });
    }
  }

  function handleKill(socket, msg) {
    const entry = ptys.get(String(msg.id));
    if (!entry) { writeFrame(socket, { ev: 'error', id: msg.id, message: 'unknown id' }); return; }
    try { entry.proc.kill(msg.signal || 'SIGTERM'); } catch (_) {}
  }

  function handleList(socket) {
    writeFrame(socket, { ev: 'list', ids: Array.from(ptys.keys()) });
  }

  const server = net.createServer((socket) => {
    let buf = '';
    let authed = false;
    socket.setEncoding('utf8');
    socket.on('data', (chunk) => {
      buf += chunk;
      let idx;
      while ((idx = buf.indexOf('\n')) !== -1) {
        const line = buf.slice(0, idx);
        buf = buf.slice(idx + 1);
        if (!line) continue;
        let msg;
        try { msg = JSON.parse(line); }
        catch (_) { writeFrame(socket, { ev: 'error', message: 'bad json' }); continue; }
        if (!authed) {
          if (msg.op === 'auth' && msg.token === opts.token) {
            authed = true;
            writeFrame(socket, { ev: 'authed' });
          } else {
            writeFrame(socket, { ev: 'error', message: 'auth required' });
            socket.end();
          }
          continue;
        }
        switch (msg.op) {
          case 'spawn': handleSpawn(socket, msg); break;
          case 'input': handleInput(socket, msg); break;
          case 'kill':  handleKill(socket, msg); break;
          case 'list':  handleList(socket); break;
          default:
            writeFrame(socket, { ev: 'error', message: 'unknown op: ' + msg.op });
        }
      }
    });
    socket.on('close', () => cleanupSocket(socket));
    socket.on('error', () => cleanupSocket(socket));
  });

  server.on('error', (e) => {
    console.error('anvil-pty-broker: listen error: ' + e.message);
    process.exit(4);
  });

  server.listen(opts.port, '127.0.0.1', () => {
    const addr = server.address();
    console.log(JSON.stringify({ ready: true, port: addr.port }));
  });

  const shutdown = () => {
    for (const [id, entry] of ptys) {
      try { entry.proc.kill(); } catch (_) {}
    }
    server.close(() => process.exit(0));
    setTimeout(() => process.exit(0), 500).unref();
  };
  process.on('SIGINT', shutdown);
  process.on('SIGTERM', shutdown);
}

start(parseArgs(process.argv.slice(2)));
