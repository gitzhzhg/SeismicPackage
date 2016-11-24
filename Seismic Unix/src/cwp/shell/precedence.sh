#! /bin/sh
#/*********************** self documentation **********************/
# PRECEDENCE - give table of C precedences from Kernighan and Ritchie
#
# Usage: precedence
#
#/**************** end self doc ********************************/

PATH=/bin:/usr/bin:/usr/ucb

exec cat <<'END'
                    TABLE of PRECEDENCES
                (Kernighan & Ritchie, page 49)


    ()   []   ->   .
    !  ~  ++  --  -  (type)  *  &  sizeof     {right to left}
    *   /   %
    +   -
    <<   >>
    <    <=    >    >=
    ==   !=
    &
    ^
    |
    &&
    ||
    ?   :    {right to left}
    =   +=   -=   etc.    {right to left}
    ,
END
