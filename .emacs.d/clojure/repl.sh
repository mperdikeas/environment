#!/usr/bin/env bash

FILE="./.clojure"

if [ -f $FILE ];
then
    java -cp $(echo $(echo ~)/.emacs.d/clojure/clojure-1.8.0.jar:$(echo ~)/environment/clojure-libs):$(echo ./lib/*.jar | tr ' ' ':'):.:$(cat $FILE) clojure.main $*
else
    java -cp $(echo $(echo ~)/.emacs.d/clojure/clojure-1.8.0.jar:$(echo ~)/environment/clojure-libs):$(echo ./lib/*.jar | tr ' ' ':'):. clojure.main $*
fi
