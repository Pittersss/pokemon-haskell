# Pokémon Haskell
<div align="center" style="font-size: 20px">
    Projeto da disciplina PLP (Paradigmas de Linguagens de Programação) <br>
    Professor: Everton L. G. Alves
</div> <br>

![Haskell](https://img.shields.io/badge/Haskell-Done-green?style=flat-square&logo=haskell&labelColor=%235D4F85) 

## Participantes
* [Aelson Júnior Araújo Diniz da Cunha](https://github.com/aelsonjrdiniz)
* [Beatriz de Souza Meneses](https://github.com/beatrizSM3)
* [Luana Lyz Araujo Rocha](https://github.com/luanalyz)
* [Pedro Henrique Malaquias da Silva](https://github.com/Pittersss)

## Instruções para a Execução
Acesse e instale o Haskell em: https://www.haskell.org/ghcup/<br>
### Instalar o Cabal
#### Windows
Baixe e instale o GHCup<br>
pacman -S mingw-w64-x86_64-pkg-config<br>
pacman -S mingw-w64-x86_64-glib2 mingw-w64-x86_64-gobject-introspection<br>
pacman -S mingw-w64-x86_64-gtk4<br>
Após a instalação execute no seu cmd:<br>
ghcup install cabal latest
ghcup set cabal latest<br>
#### Linux (Ubuntu)

sudo apt update<br>
sudo apt-get install pkg-config<br>
sudo apt install libgtk-4-dev gobject-introspection libgirepository1.0-dev<br>
sudo apt-get install pkg-config libglib2.0-dev libgobject-2.0-dev<br>
sudo apt install cabal-install
<br>
#### MacOs
brew install pkg-config glib gobject-introspection gtk4<br>
Se houver problemas com o pkg-config não encontrando as bibliotecas, adicione o seguinte ao seu ambiente:<br>
export PKG_CONFIG_PATH="$(brew --prefix)/lib/pkgconfig:$(brew --prefix)/share/pkgconfig"<br>
<br>

brew install cabal-install

### Abra o diretório do projeto e execute:
cabal update<br>
cabal install --only-dependencies<br>
cabal run
