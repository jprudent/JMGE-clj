Dans cet article je vais diffamer javascript.
Attendez-vous à de la violence gratuite et non justifiée.

Ultra-Mutabilité
================

En javascript, l'état des objets est mutable.

Les fonctions étant des citoyens de première classe,
on peut muter le comportement d'un objet

      function Mammal(){
      };

      Mammal.prototype.breath = function(){
        return "breathing";
      };

      var m1 = new Mammal();

      m1 // Mammal {breath: function}

En réalité `breath` est portée par le `prototype` de `m1`.

      m1.hasOwnProperty("breath"); // false
      m1.__proto__ // Mammal {breath: function}

On peut surcharger `breath` dans l'objet lui même

