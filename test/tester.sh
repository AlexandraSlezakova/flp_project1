#!/usr/bin/env bash
#title         : tester.sh
#description   : Test correctness of dka-2-mka.hs
#author        : Alexandra Slezakova (xsleza20)
#Year          : 2021
#usage         : ./tester [OPTION] see --help
#=================================================

RED='\033[0;31m'
BPur='\e[1;35m';
NC=$'\e[0m'
GREEN=$'\e[0;32m'

print_dfa()
{
  echo "----------------------------------------------------------"
  echo -e "${BPur}Tests with correctly written deterministic finite automata${NC}"
  echo "----------------------------------------------------------"
  for ((i = 0 ; i < 6; i++)); do
        echo "File: test0${i}.in"
        CMD="${FILE} -i correct_dfa/test0${i}.in"
        eval OUTPUT=\$\($CMD\)
        if [[ $(< "correct_dfa/test0${i}".out) != "$OUTPUT" ]]; then
          echo -e "Result: ${RED}the program output and the required output are different${NC}"
        else
          echo "Result: ${GREEN}OK${NC}"
        fi

        if [[ ${i} -lt 5 ]]; then
          echo "----------------"
        fi

  done

  echo "------------------------------------------------------------"
  echo -e "${BPur}Tests with incorrectly written deterministic finite automata${NC}"
  echo "------------------------------------------------------------"
  SUBSTRING='error'
  errors=("Input symbol on transition is not in alpabet"
          "Transition with epsilon"
          "Accept state is not in set of states"
          "Initial state is not in set of states"
          "Source state of transition is not in set of states"
          "Destination state of transition is not in set of states"
          "No destination state of transition"
          "No source state of transition"
          "Accept states are not in set of states"
          "Alphabet contains number"
          "Aplhabet contains uppercase letter"
          "Set of states contains letter"
          "Negative number in set of states"
          "No initial state"
          "Empty set of accept states"
          "Duplicates in set of states"
          "Duplicates in set of transitions")

  for ((i = 0 ; i < 17; i++)); do
        if [[ ${i} -gt 9 ]]; then
          INDEX="${i}"
        else
          INDEX="0${i}"
        fi

        echo "File: test${INDEX}-bad.in"
        CMD="${FILE} -i correct_dfa/test${INDEX}-bad.in 2>&1"
        eval OUTPUT=\$\($CMD\)
        if [[ "$OUTPUT" == *"$SUBSTRING"* ]]; then
          echo "Result: ${GREEN}OK - error found${NC}"
        else
          echo -e "Result: ${RED}no error was found in the deterministic finite automata ${NC}"
        fi
        echo "Error: ${errors[$i]}"

        if [[ ${i} -lt 16 ]]; then
          echo "----------------"
        fi

  done
}

reduce_dfa()
{
  echo "-----------------------------------------------------------------------"
  echo -e "${BPur}Minimization of DFA - tests with complete deterministic finite automata${NC}"
  echo "-----------------------------------------------------------------------"
  for ((i = 0 ; i < 18; i++)); do
    if [[ ${i} -gt 9 ]]; then
      INDEX="${i}"
    else
      INDEX="0${i}"
    fi

    echo "File: test${INDEX}.in"
    CMD="${FILE} -t reduced_dfa/complete_dfa/test${INDEX}.in"
    eval OUTPUT=\$\($CMD\)
    if [[ $(< "reduced_dfa/complete_dfa/test${INDEX}".out) != "$OUTPUT" ]]; then
      echo -e "Result: ${RED}the program output and the required output are different${NC}"
    else
      echo "Result: ${GREEN}OK${NC}"
    fi

    if [[ ${i} -lt 17 ]]; then
      echo "----------------"
    fi
  done


  echo "-------------------------------------------------------------------------"
  echo -e "${BPur}Minimization of DFA - tests with incomplete deterministic finite automata${NC}"
  echo "-------------------------------------------------------------------------"
  for ((i = 0 ; i < 8; i++)); do
    if [[ ${i} -gt 9 ]]; then
      INDEX="${i}"
    else
      INDEX="0${i}"
    fi

    echo "File: test${INDEX}.in"
    CMD="${FILE} -t reduced_dfa/incomplete_dfa/test${INDEX}.in"
    eval OUTPUT=\$\($CMD\)
    if [[ $(< "reduced_dfa/incomplete_dfa/test${INDEX}".out) != "$OUTPUT" ]]; then
      echo -e "Result: ${RED}the program output and the required output are different${NC}"
    else
      echo "Result: ${GREEN}OK${NC}"
    fi

    if [[ ${i} -lt 7 ]]; then
      echo "----------------"
    fi

  done
}

help()
{
    echo "Usage: ./tester.sh [OPTION]"
    echo -e "\t-i test correctness of saving DKA to the internal representation and print it to stdout"
    echo -e "\t-t create and print MKA to stdout"
}

FILE=../dka-2-mka
if [ ! -f "$FILE" ]; then
    echo "Error: Executable file $FILE doesn't exist."
    exit 0
fi

if [ -z "$1" ]; then
    help
    exit 0
fi

if [ "$1" == "--help" ]; then
   help
   exit 0
elif [ "$1" == "-i" ]
then
    print_dfa
elif [ "$1" == "-t" ]
then
    reduce_dfa
fi
