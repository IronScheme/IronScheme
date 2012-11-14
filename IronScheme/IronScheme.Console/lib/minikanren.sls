#!r6rs
(library (minikanren)
(export run
        succeed
        fail
        ==
        ==-check
        fresh
        conde
        all
        alli
        condi
        conda
        condu
        ife
        ifi
        ifa
        ifu
        run*
        lambda-limited
       
        caro
        cdro
        conso
        nullo
        eqo
        eq-caro
        pairo
        listo
        membero
        rembero
        appendo
        anyo
        nevero
        alwayso

        build-num
        poso
        >1o
        +o
        -o
        *o
        =lo
        <lo
        <=lo
        <o
        <=o
        /o
        logo
        expo
        trace-vars
        )
(import (minikanren mk)
        (minikanren mkextraforms)
        (minikanren mkprelude))
)
