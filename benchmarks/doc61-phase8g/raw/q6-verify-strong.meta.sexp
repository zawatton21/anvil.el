(:qid "q6" :arm "verify-strong" :wall-sec 1220 :cost-usd nil :errored t
      :error-msg "TWO attempts (initial + 1 retry), each killed by the
outer `timeout 1220` (~20min cap) with zero output past the initial start
message. Not a harness/module bug (q1-q5 verify-strong all completed
normally in 4-16min on the identical code path); most likely an unusually
slow sonnet-tier sub-task (member, skeptic, or judge) for this specific
question's prompt/claim set. Recorded as errored per spec instruction
rather than retried a third time. Sub-task spend before each kill is not
recoverable (in-memory cost-capture advice list died with the process);
treat as unknown, not zero, when reading total cost."
      :claims nil :panel sonnet-solo :judge-model "claude-sonnet-5"
      :rounds nil)
