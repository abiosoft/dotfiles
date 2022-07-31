FROM ubuntu:disco

RUN apt update && apt install -y \
        neovim \
        git \
        curl \
        wget \
        python-pip \
        python3-pip \
        ctags \
        zsh \
        command-not-found \
        htop \
        tree


RUN pip2 install --user neovim autopep8 pylink rope --upgrade
RUN pip3 install --user neovim autopep8 pylink rope --upgrade

RUN git clone https://github.com/abiosoft/dotfiles /root/dotfiles
RUN cd /root/dotfiles/neovim && ./install.sh
RUN cd /root/dotfiles/zsh && ./install.sh

CMD /bin/zsh

