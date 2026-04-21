FROM archlinux:latest

ENV LANG=C.UTF-8 \
    LC_ALL=C.UTF-8

RUN pacman -Sy --noconfirm \
      emacs-nox \
      git \
      python \
      ca-certificates \
      curl \
      bash \
      jq \
 && pacman -Scc --noconfirm

RUN useradd -m -s /bin/bash testuser
USER testuser
WORKDIR /home/testuser
ENV HOME=/home/testuser

CMD ["bash", "/anvil/scripts/test-install-container.sh"]
