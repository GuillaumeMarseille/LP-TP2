(*******************************************************************)
(* Langages de Programmation: IFT 3000 NRC 54797                   *)
(* TP2 ÉTÉ 2022. Date limite: Jeudi 14 juillet à 17h00             *)
(* Manipulation d'un plan de distribution de marchandise           *)
(*******************************************************************)
(*                                                                 *)
(* NOM: _______________________ PRÉNOM:___________________________ *)
(* MATRICULE: _________________ PROGRAMME: _______________________ *)
(*                                                                 *)
(*******************************************************************)

(* Chargement de la signature du TP2 *)
#use "TP2-SIG-E2022.mli";;


module Tp2e22 : TP2E22 = struct

  open List
  open Sys
  open Printf

  (* Classe client *)
  class  client (nc: string) (dc: int) (co: float*float) =
  object
    val nom = nc
    val demande = dc
    val coordonnees = co
    val mutable distance = 0.0
    method get_nom = nom
    method get_demande = demande
    method get_coordonnees = coordonnees
    method get_distance = distance
    method set_distance (d: float) = distance <- d
  end


  (* Classe itineraire *)
  class itineraire =
  let num = ref 1 in
    fun (c : int) ->
  object(self)
    val numero = !num
    val capacite = c
    val mutable demande_totale = 0
    val mutable distance_totale = 0.0
    val mutable liste_clients : client list = []
    method get_numero = numero
    method get_capacite = capacite
    method get_demande_totale = demande_totale
    method get_distance_totale = distance_totale
    method get_liste_clients = liste_clients
    method set_demande_totale (d: int) = demande_totale <- d
    method set_distance_totale (dis: float) = distance_totale <- dis
    method set_liste_clients  (l: client list) = liste_clients <- l
    initializer
      num := !num + 1

    (* Méthodes à implanter *)

   (* -- À IMPLANTER (3 PTS) -------------------------------------------*)
   (* @Méthode : client_existe : string -> bool                         *)
   (* @Description : Détermine si un client existe dans l'itinéraire    *)

    method client_existe (nomc: string) =
		true

   (* -- À IMPLANTER (4 PTS) -------------------------------------------*)
   (* @Méthode : retourner_client : string -> client                    *)
   (* @Description : Détermine si un client existe dans l'itinéraire    *)
   (* @Exception: Lance l'exception Failure si le client n'existe pas   *)

    method retourner_client (nomc: string) =
		new client "" 0 (0.0, 0.0)

   (* -- À IMPLANTER (10 PTS) ------------------------------------------*)
   (* @Méthode : ajouter_client : client -> bool -> unit                *)
   (* @Description : Ajoute un client dans l'itinéraire                 *)
   (* @Exception: Lance l'exception Failure si le client existe déjà    *)
   (* @Exception: Lance l'exception Failure si la capacité de           *)
   (*             l'itinéraire ne permet pas d'ajouter le client.       *)

    method ajouter_client (c: client) (b: bool) =
		()

   (* -- À IMPLANTER (8 PTS) -------------------------------------------*)
   (* @Méthode : supprimer_client : string -> unit                      *)
   (* @Description : Supprime un client d'un itinéraire                 *)
   (* @Exception: Lance l'exception Failure si le client n'existe pas   *)

    method supprimer_client (nomc: string) =
		()

   (* -- À IMPLANTER (5 PTS) ----------------------------------------------------------*)
   (* @Méthode : ajouter_clients: (string * int * (float * float) * bool) list -> unit *)
   (* @Description : Ajoute plusieurs clients dans l'itinéraire selon                  *)
   (*				 les informations reçues                               *)

    method ajouter_clients (ilc: (string * int * (float * float) * bool) list) =
		()

   (* -- À IMPLANTER (5 PTS) ----------------------------------*)
   (* @Méthode : afficher_itineraire : unit                    *)
   (* @Description : Affiche les informations de l'itinéraire  *)

    method afficher_itineraire =
		()

  end

  (* Classe plan *)
  class plan (sp: string) =
  object
    val sorte_plan : string = sp
    method get_sorte_plan = sorte_plan
  end

  (* Classe plan_distribution *)
  class plan_distribution (sp: string) (np: string) =
  object(self)
    inherit plan sp as parent
    val nom_plan : string = np
    val mutable liste_itineraires : itineraire list = []
    method get_nom_plan = nom_plan
    method get_liste_itineraires = liste_itineraires
    method set_liste_itineraires (l : itineraire list) = liste_itineraires <- l

    (* Méthodes à implanter *)

   (* -- À IMPLANTER (3 PTS) -------------------------------------------*)
   (* @Méthode : itineraire_existe: int -> bool                         *)
   (* @Description : Détermine si un itinéraire existe dans le plan     *)

    method itineraire_existe (indice: int) =
		true

   (* -- À IMPLANTER (4 PTS) -------------------------------------------*)
   (* @Méthode : ajouter_itineraire: itineraire -> unit                 *)
   (* @Description : Ajoute un intinéraire dans le plan                 *)
   (* @Exception: Lance l'exception Failure si l'itinéraire existe déjà *)

    method  ajouter_itineraire (i: itineraire) =
		()

   (* -- À IMPLANTER (4 PTS) -------------------------------------------*)
   (* @Méthode : retourner_liste_clients: client list                   *)
   (* @Description : Retourne la liste de tous les clients du plan      *)

    method retourner_liste_clients =
		[new client "" 0 (0.0, 0.0)]

   (* -- À IMPLANTER (4 PTS) -------------------------------------------*)
   (* @Méthode : client_existe : string -> bool                         *)
   (* @Description : Détermine si un client existe dans le plan         *)

    method client_existe (nomc: string) =
		true

   (* -- À IMPLANTER (5 PTS) ---------------------------------------------------------------------------*)
   (* @Méthode : ajouter_itineraires: int list -> (string * int (float*float) * bool) list list -> unit *)
   (* @Description : Ajoute plusieurs itinéraires dans le plan selon les informations reçues            *)

    method ajouter_itineraires (lcap: int list) (lilc: (string * int * (float*float) * bool) list list) =
		()

   (* -- À IMPLANTER (6 PTS) ------------------------------------------------------*)
   (* @Méthode : retourner_ICRE: (int * int)                                       *)
   (* @Description : Retourne le numéro ainsi que la capacité résiduelle de        *)
   (*                l’itinéraire ayant cette capacité la plus élevée dans le plan *)
   (* @Exception: Lance l'exception Failure si le plan est vide                    *)

    method retourner_ICRE =
		(0,0)

   (* -- À IMPLANTER (4 PTS) -------------------------------------------*)
   (* @Méthode : retourner_client : string -> client                    *)
   (* @Description : Détermine si un client existe dans le plan         *)
   (* @Exception: Lance l'exception Failure si le client n'existe pas   *)

    method retourner_client (nomc: string) =
		new client "" 0 (0.0, 0.0)

   (* -- À IMPLANTER (4 PTS) --------------------------------------------*)
   (* @Méthode : retourner_itineraire : int -> itineraire                *)
   (* @Description : Retourne l'itinéraire se trouvant dans le plan      *)
   (* @Exception: Lance l'exception Failure si l'itinéraire n'existe pas *)

    method retourner_itineraire (indice: int) =
		new itineraire 0

   (* -- À IMPLANTER (3 PTS) ------------------------------------------*)
   (* @Méthode : calculer_distance_totale : float                      *)
   (* @Description : Calcul et retourne la distance totale du plan en  *)
   (*                faisant la somme de toutes les distances des      *)
   (*				 itinéraires appartenant au plan       *)

    method calculer_distance_totale =
		0.0

   (* -- À IMPLANTER (6 PTS) --------------------------------------------------*)
   (* @Méthode : afficher_plan_distribution1: unit                             *)
   (* @Description : Affiche le plan dans l’interpréteur d’Ocaml (voir énoncé) *)
   (* @Exception: Lance l'exception Failure si le plan est vide                *)

    method afficher_plan_distribution1 =
		()

   (* -- À IMPLANTER (12 PTS) -------------------------------------------------*)
   (* @Méthode : afficher_plan_distribution2: string -> string -> unit         *)
   (* @Description : Affiche le plan dans une image jpg (voir énoncé)          *)
   (* @Exception: Lance l'exception Failure si le plan est vide                *)

    method afficher_plan_distribution2 (file: string) (image: string) =
		()

  end

end