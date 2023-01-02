%%%%%%%%% Definitions
piece(Animal,Player):-
    animal(Animal),
    (Player = 1; Player = 2).

animal(l). %lion
animal(e). %elephant
animal(r). %rat

water(w, (Animal, Player)):-
        (animal(Animal) ; Animal = o),
        (Player = 0; Player = 1; Player = 2).

scared_of(l,e).
scared_of(e,r).
scared_of(r,l).

movement(l, diagonal).
movement(r, orthogonal).
movement(e, both).
%%%%%%%%% Definitions