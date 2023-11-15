FROM mcr.microsoft.com/devcontainers/base:ubuntu

RUN apt update
RUN apt install -y graphviz libfreetype6 libxi6 libxtst6 libxrender1 libxext6

USER vscode

RUN curl -s "https://get.sdkman.io" | bash
RUN chmod +x "$HOME/.sdkman/bin/sdkman-init.sh"
RUN "$HOME/.sdkman/bin/sdkman-init.sh"

RUN zsh -c "source $HOME/.sdkman/bin/sdkman-init.sh && sdk install java 17-open"
RUN zsh -c "source $HOME/.sdkman/bin/sdkman-init.sh && sdk install scala 3.3.1"
RUN zsh -c "source $HOME/.sdkman/bin/sdkman-init.sh && sdk install sbt"