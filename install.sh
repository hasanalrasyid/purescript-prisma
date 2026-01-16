echo "source ${HOME}/.nvm/nvm.sh
nvm use $(node -v) >& /dev/null
node $(pwd)/Main.mjs " '$@' > ${HOME}/.local/bin/prismaParser

chmod +x ${HOME}/.local/bin/prismaParser
