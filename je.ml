(***************************************************************************)
(* Jeu d'essai - TP2 - �t� 2022                                            *)
(***************************************************************************)

(* On charge le fichier ml du Tp apr�s avoir implant�
les fonctions demand�es pour realiser les tests  *)

#use "TP2-E2022.ml";;

(* R�sultat:
module type TP2E22 =
  sig
    class client :
      string ->
      int ->
      float * float ->
      object
        val coordonnees : float * float
        val demande : int
        val mutable distance : float
        val nom : string
        method get_coordonnees : float * float
        method get_demande : int
        method get_distance : float
        method get_nom : string
        method set_distance : float -> unit
      end
    class itineraire :
      int ->
      object
        val capacite : int
        val mutable demande_totale : int
        val mutable distance_totale : float
        val mutable liste_clients : client list
        val numero : int
        method afficher_itineraire : unit
        method ajouter_client : client -> bool -> unit
        method ajouter_clients :
          (string * int * (float * float) * bool) list -> unit
        method client_existe : string -> bool
        method get_capacite : int
        method get_demande_totale : int
        method get_distance_totale : float
        method get_liste_clients : client list
        method get_numero : int
        method retourner_client : string -> client
        method set_demande_totale : int -> unit
        method set_distance_totale : float -> unit
        method set_liste_clients : client list -> unit
        method supprimer_client : string -> unit
      end
    class plan :
      string ->
      object val sorte_plan : string method get_sorte_plan : string end
    class plan_distribution :
      string ->
      string ->
      object
        val mutable liste_itineraires : itineraire list
        val nom_plan : string
        val sorte_plan : string
        method afficher_plan_distribution1 : unit
        method afficher_plan_distribution2 : string -> string -> unit
        method ajouter_itineraire : itineraire -> unit
        method ajouter_itineraires :
          int list ->
          (string * int * (float * float) * bool) list list -> unit
        method calculer_distance_totale : float
        method client_existe : string -> bool
        method get_liste_itineraires : itineraire list
        method get_nom_plan : string
        method get_sorte_plan : string
        method itineraire_existe : int -> bool
        method retourner_ICRE : int * int
        method retourner_client : string -> client
        method retourner_itineraire : int -> itineraire
        method retourner_liste_clients : client list
        method set_liste_itineraires : itineraire list -> unit
      end
  end
module Tp2e22 : TP2E22
*)

(* On ouvre le modules disposant de fonctions pertinentes pour nos tests *)
open Tp2e22;;
(* On ex�cute maintenant les fonctions une � une *)
let c1= new client "c7" 10 (23.5, 15.9);;

(* R�sultat:
val c1 : Tp2e22.client = <obj>
*)

let i1 = new itineraire 90;;

(* R�sultat:
val i1 : Tp2e22.itineraire = <obj>
*)

(* Utilisation de dc ou pdc pour savoir si on ajoute le dernier client � l'itin�raire *)
let dc = true and pdc = false;;
(* R�sultat:
val dc : bool = true
val pdc : bool = false
*)

i1#ajouter_client c1 pdc;;

(* R�sultat:
- : unit = ()
*)

i1#get_demande_totale;;
i1#get_distance_totale;;

(* R�sultat:
- : int = 10
- : float = 28.373579259585842
*)

i1#afficher_itineraire;;

(* R�sultat:
DemandeTotale: 10; DistanceTotale: 28.3735792596; Clients: c7
- : unit = ()
*)

i1#client_existe "c7";;

(* R�sultat:
- : bool = true
*)

i1#retourner_client "c7";;

(* R�sultat:
- : Tp2e22.client = <obj>
*)

i1#ajouter_clients [("c14",5,(42.1,27.2),pdc);
                    ("c22",12,(32.5,39.9),pdc);
                    ("c3",7,(8.1,17.2),pdc);
                    ("c11",4,(36.5,45.7),pdc);
                    ("c23",13,(18.9,25.7),dc)];;

(* R�sultat:
- : unit = ()
*)

i1#afficher_itineraire;;

(* R�sultat:
DemandeTotale: 51; DistanceTotale: 198.160779731; Clients: c7 c14 c22 c3 c11 c23
- : unit = ()
*)

let p = new plan_distribution "Plan distribution" "Transport marchandise";;

(* R�sultat:
val p : Tp2e22.plan_distribution = <obj>
*)

p#ajouter_itineraire i1;;
(* R�sultat:
- : unit = ()
*)

p#itineraire_existe 1;;

(* R�sultat:
- : bool = true
*)

p#ajouter_itineraires [115;70;150]
                      [[("c13",8,(26.7,3.9),pdc);("c1",7,(14.3,23.5),pdc);
                        ("c2",3,(10.6,27.8),pdc);("c10",11,(21.1,5.7),pdc);
                        ("c20",5,(4.9,33.6),pdc);("c19",15,(19.1,37.2),dc)];
                      [("c4",12,(32.5,39.9),pdc);("c18",10,(18.9,25.7),pdc);
                       ("c8",14,(28.6,7.1),pdc);("c16",8,(4.3,31.5),dc)];
                      [("c6",9,(10.6,27.8),pdc);("c12",3,(41.1,15.2),pdc);
                       ("c17",6,(19.1,37.2),pdc);("c21",8,(14.3,23.5),pdc);
                       ("c5",10,(21.1,5.7),pdc);("c15",4,(16.2,8.9),pdc);
                       ("c9",7,(43.7,12.7),dc)]];;

(* R�sultat:
 - : unit = ()
*)

p#retourner_liste_clients;;

(* R�sultat:
- : Tp2e22.client list =
[<obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
 <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>; <obj>;
 <obj>]
*)

p#client_existe "c5";;

(* R�sultat:
- : bool = true
*)

p#retourner_ICRE;;

(* R�sultat:
- : int * int = (4, 103)
*)

p#retourner_client "c21";;

(* R�sultat:
- : Tp2e22.client = <obj>
*)

p#retourner_itineraire 3;;

(* R�sultat:
- : Tp2e22.itineraire = <obj>
*)

p#calculer_distance_totale;;

(* R�sultat:
- : float = 732.09289275072513
*)

p#afficher_plan_distribution1;;

(* R�sultat:
Nom du plan: Transport marchandise
Nombre des itineraires: 4
Nombre des clients: 23
Liste des itineraires:
it 1: DemandeTotale: 51; DistanceTotale: 198.160779731; Clients: c7 c14 c22 c3 c11 c23
it 2: DemandeTotale: 49; DistanceTotale: 169.045003635; Clients: c13 c1 c2 c10 c20 c19
it 3: DemandeTotale: 44; DistanceTotale: 158.329072921; Clients: c4 c18 c8 c16
it 4: DemandeTotale: 47; DistanceTotale: 206.558036463; Clients: c6 c12 c17 c21 c5 c15 c9
- : unit = ()
*)

p#afficher_plan_distribution2 "plan.dot" "plan.jpg";;

(* R�sultat:
Affichage du plan dans un fichier image jpg
- : unit = ()
*)

i1#supprimer_client "c14";;

(* R�sultat:
- : unit = ()
*)

i1#afficher_itineraire;;

(* R�sultat:
DemandeTotale: 46; DistanceTotale: 186.109176346; Clients: c7 c22 c3 c11 c23
- : unit = ()
*)

(***************************************)
(* Verification des messages d'erreurs *)
(***************************************)

try
    ignore(i1#retourner_client "c99")
with
    Failure s -> print_endline s;;

(* R�sultat:
Ce client n'existe pas.
- : unit = ()
*)

try
    i1#ajouter_client c1 pdc
with
    Failure s -> print_endline s;;

(* R�sultat:
Ce client existe d�j�.
- : unit = ()
*)

try
    i1#ajouter_client (new client "c99" 500 (12.2, 42.1)) pdc
with
    Failure s -> print_endline s;;

(* R�sultat:
La capacit� de cet itin�raire ne permet pas d'ajouter ce client.
- : unit = ()
*)

try
    i1#supprimer_client "c99"
with
   Failure s -> print_endline s;;

(* R�sultat:
Ce client n'existe pas.
- : unit = ()
*)

try
    ignore((new plan_distribution "test" "test")#retourner_ICRE)
with
  Failure s -> print_endline s;;

(* R�sultat:
Le plan est vide.
- : unit = ()
*)

try
    ignore(p#retourner_client "c99")
with
    Failure s -> print_endline s;;

(* R�sultat:
Ce client n'existe pas.
- : unit = ()
*)

try
    ignore(p#retourner_itineraire 5)
with
    Failure s -> print_endline s;;

(* R�sultat:
L'itin�raire n'existe pas.
- : unit = ()
*)

try
    (new plan_distribution "test" "test")#afficher_plan_distribution1
with
    Failure s -> print_endline s;;

(* R�sultat:
Le plan est vide.
- : unit = ()
*)

try
    (new plan_distribution "test" "test")#afficher_plan_distribution2 "test" "test"
with
    Failure s -> print_endline s;;

(* R�sultat:
Le plan est vide.
- : unit = ()
*)
