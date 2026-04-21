# Ubuntu 24.04 with Emacs pre-installed.
# Exercises install.sh on the --skip-emacs path (no package-manager install
# inside the container).  The auto-install path (--fresh) is left to a
# separate Dockerfile to keep this one fast (~15s rebuild vs. ~5min).
FROM ubuntu:24.04

ENV DEBIAN_FRONTEND=noninteractive \
    LANG=C.UTF-8 \
    LC_ALL=C.UTF-8 \
    TZ=UTC

RUN apt-get update \
 && apt-get install -y --no-install-recommends \
      emacs-nox \
      git \
      python3 \
      ca-certificates \
      curl \
      bash \
      jq \
 && rm -rf /var/lib/apt/lists/*

RUN useradd -m -s /bin/bash testuser
USER testuser
WORKDIR /home/testuser
ENV HOME=/home/testuser

CMD ["bash", "/anvil/scripts/test-install-container.sh"]
