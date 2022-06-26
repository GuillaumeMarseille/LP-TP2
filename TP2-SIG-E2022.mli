(*****************************************************************)
(* Langages de Programmation: IFT 3000 NRC 54797                 *)
(* TP2 Été 2022. Date limite: Jeudi 14 juillet à  17h00          *)
(* Manipulation d'un plan de distribution de marchandise         *)
(*****************************************************************)
(* Signature du TP2 (4 Classes) 		                 *)
(*****************************************************************)

module type TP2E22 = sig

  (* Classe client *)
  class client : string -> int -> float*float ->
  object
    val nom : string
    val demande : int
    val coordonnees : float*float
    val mutable distance : float
    method get_nom : string
    method get_demande : int
    method get_coordonnees : float*float
    method get_distance : float
    method set_distance : float -> unit
  end

  (* Classe itineraire *)
  class itineraire : int ->
  object
    val numero : int
    val capacite : int
    val mutable demande_totale : int
    val mutable distance_totale : float
    val mutable liste_clients : client list
    method get_numero : int
    method get_capacite : int
    method get_demande_totale : int
    method get_distance_totale : float
    method get_liste_clients : client list
    method set_demande_totale : int -> unit
    method set_distance_totale : float -> unit
    method set_liste_clients : client list -> unit
    method client_existe : string -> bool
    method retourner_client : string -> client
    method ajouter_client : client -> bool -> unit
    method supprimer_client : string -> unit
    method ajouter_clients : (string * int * (float * float)* bool) list -> unit
    method afficher_itineraire : unit
  end

  (* Classe plan *)
  class plan : string ->
  object
    val sorte_plan : string
    method get_sorte_plan : string
  end

  (* Classe plan_distribution *)
  class plan_distribution : string -> string ->
  object
    inherit plan
    val nom_plan : string
    val mutable liste_itineraires : itineraire list
    method get_nom_plan : string
    method get_liste_itineraires : itineraire list
    method set_liste_itineraires : itineraire list -> unit
    method itineraire_existe : int -> bool
    method ajouter_itineraire : itineraire -> unit
    method retourner_liste_clients : client list
    method client_existe : string -> bool
    method ajouter_itineraires : int list -> (string * int * (float*float) * bool) list list -> unit
    method retourner_ICRE : (int * int)
    method retourner_client : string -> client
    method retourner_itineraire : int -> itineraire
    method calculer_distance_totale : float
    method afficher_plan_distribution1 : unit
    method afficher_plan_distribution2 : string -> string -> unit

  end

end
