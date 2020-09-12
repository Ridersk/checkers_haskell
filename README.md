# Jogo de Damas

**Disciplina**: FGA0210 - PARADIGMAS DE PROGRAMAÇÃO - T01  
**Nro do Grupo**: 01  
**Paradigma**: Funcional  

## Alunos

|Matrícula | Aluno |
| -- | -- |
| 15/0064535 | Rodrigo Dadamos |
| 15/0128312 | Guilherme Lima Matos Leal |
| 16/0013321 | Luciano dos Santos Silva |
| 17/0070735 | Lucas Maciel Aguiar |

## Sobre  

Jogo de Tabuleiro Simples

## Screenshots

Adicione 2 ou mais screenshots do projeto em termos de interface e/ou funcionamento.

## Instalação

**Linguagens**: Haskell
**Tecnologias**: Gtk2

## Uso  

* Instalar Dependências do Sistema

      sudo apt install libgtk2.0-dev

* Criar Sandbox - Cabal

      cabal sandbox init

* Instalar Dependências do Projeto

      cabal install

* Build do Projeto

      cd src
      cabal sandbox --sandbox=../.cabal-sandbox/ init
      cabal exec -- ghc --make Main.hs

* Execução

      ./Main

## Vídeo  

Adicione 1 ou mais vídeos com a execução do projeto.

## Outros  

Quaisquer outras informações sobre seu projeto podem ser descritas a seguir.

## Fontes

* __Repositório com módulos úteis para construção de jogos de tabuleiro simples (Keera-studios):__ <https://github.com/keera-studios/gtk-helpers>
