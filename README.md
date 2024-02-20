dnf install stack
dnf install haskell-server

mkdir -p $HOME/.local/bin
cp xmonad.hs to $HOME/.config/xmonad
cd &Home/config/xmonad

stack upgrade
stack init
stack install
