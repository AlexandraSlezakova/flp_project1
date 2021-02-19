#!/usr/bin/env bash
#title         : tester.sh
#description   : Test correctness of dka-2-mka.hs
#author        : Alexandra Slezakova
#usage         : ./tester [OPTION] see --help
#==============================================================================
RED='\033[0;31m'
BPur='\e[1;35m';
NC=$'\e[0m'
GREEN=$'\e[0;32m'

print_dka()
{
  echo "---------------------------------------------------------"
  echo -e "${BPur}Tests with correctly written Deterministic finite automata${NC}"
  echo "---------------------------------------------------------"
  for ((i = 0 ; i < 6; i++)); do
        echo "File: test0${i}.in"
        CMD="../dka-2-mka -i test0${i}.in"
        eval OUTPUT=\$\($CMD\)
        if [[ $(< "test0${i}".out) != "$OUTPUT" ]]; then
          echo -e "Result: ${RED}the program output and the required output are different${NC}"
        else
          echo "Result: ${GREEN}OK${NC}"
        fi

        if [[ ${i} -lt 5 ]]; then
          echo "----------------"
        fi

  done

  echo "---------------------------------------------------------"
  echo -e "${BPur}Tests with incorrectly written deterministic finite automata${NC}"
  echo "---------------------------------------------------------"
  SUBSTRING='Invalid input'
  errors=("Input symbol on transition is not in alpabet"
          "Transition with epsilon"
          "Accept state is not in set of states"
          "Initial state is not in set of states"
          "Source state of transition is not in set of states"
          "Destination state of transition is not in set of states"
          "No destination state of transition"
          "No source state of transition"
          "Accept states are not in set od states"
          "Alphabet contains number"
          "Aplhabet contains uppercase letter"
          "Set of states contains letter"
          "Negative number in set of states")

  for ((i = 0 ; i < 13; i++)); do
        if [[ ${i} -gt 9 ]]; then
          INDEX="${i}"
        else
          INDEX="0${i}"
        fi

        echo "File: test${INDEX}-bad.in"
        CMD="../dka-2-mka -i test${INDEX}-bad.in 2>&1"
        eval OUTPUT=\$\($CMD\)
        if [[ "$OUTPUT" == *"$SUBSTRING"* ]]; then
          echo "Result: ${GREEN}OK${NC}"
        else
          echo -e "Result: ${RED}no error was found in the deterministic finite automata ${NC}"
        fi
        echo "Error: ${errors[$i]}"

        if [[ ${i} -lt 12 ]]; then
          echo "----------------"
        fi

  done
}

help()
{
    echo "Usage: ./tester.sh [OPTION]"
    echo -e "\t-i test correctness of saving DKA to custom representation and print it to stdout"
    echo -e "\t-t print MKA to stdout"
}

if [ -z "$1" ]; then
    help
    exit 0
fi

if [ "$1" == "--help" ]; then
   help
   exit 0
elif [ "$1" == "-i" ]
then
    print_dka
elif [ "$2" == "-t" ]
then
    echo "este nie je"
fi
