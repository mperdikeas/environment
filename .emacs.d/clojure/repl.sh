#java -cp ~/environment/clojure-libs -jar ~/.emacs.d/clojure/clojure-1.4.0.jar
java -cp $(echo $(echo ~)/.emacs.d/clojure/clojure-1.4.0.jar:$(echo ~)/environment/clojure-libs) clojure.main