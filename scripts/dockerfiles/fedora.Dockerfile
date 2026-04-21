FROM fedora:40

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

RUN dnf install -y \
      emacs-nox \
      git \
      python3 \
      ca-certificates \
      curl \
      bash \
      jq \
 && dnf clean all

RUN useradd -m -s /bin/bash testuser
USER testuser
WORKDIR /home/testuser
ENV HOME=/home/testuser

CMD ["bash", "/anvil/scripts/test-install-container.sh"]
