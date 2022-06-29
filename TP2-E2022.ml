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

    (*methode prive pour retourner les coords du dernier client de la liste*)
    method private coord_dernier_client =
      let l = rev self#get_liste_clients in
      let c = hd l in
      c#get_coordonnees

    (*methode pour calculer distance entre 2 coords*)
    method private calculer_distance (c1: float*float) (c2: float*float) =
      sqrt ((fst c2 -. fst c1)**2.0 +. (snd c2 -. snd c1)**2.0)

    (*methode pour mettre a jours la distance des elements dans une liste*)
    method private maj_distance_clients l = 
	let prev = ref (0.0, 0.0) in
	iter (fun c -> c#set_distance (self#calculer_distance c#get_coordonnees !prev); prev := c#get_coordonnees) l



   (* -- À IMPLANTER (3 PTS) -------------------------------------------*)
   (* @Méthode : client_existe : string -> bool                         *)
   (* @Description : Détermine si un client existe dans l'itinéraire    *)

    method client_existe (nomc: string) =
       (*definition de fonction recursive*)
      let rec rec_client_existe (n: string) (l: client list) =
        match l with
                |[] -> false
                |e::r -> (n = e#get_nom) || rec_client_existe n r in
       (*appel de la fonction recursive avec les bon parametres*)
      rec_client_existe nomc (self#get_liste_clients)

   (* -- À IMPLANTER (4 PTS) -------------------------------------------*)
   (* @Méthode : retourner_client : string -> client                    *)
   (* @Description : Détermine si un client existe dans l'itinéraire    *)
   (* @Exception: Lance l'exception Failure si le client n'existe pas   *)

    method retourner_client (nomc: string) =
             (*definition de fonction recursive*)
      let rec rec_retourner_client (n: string) (l: client list) =
        match l with
                |[] -> failwith "Ce client n'existe pas."
                |e::r -> if (e#get_nom = n) then e else rec_retourner_client n r in
       (*appel de la fonction recursive avec les bon parametres*)
      rec_retourner_client nomc (self#get_liste_clients)
      

   (* -- À IMPLANTER (10 PTS) ------------------------------------------*)
   (* @Méthode : ajouter_client : client -> bool -> unit                *)
   (* @Description : Ajoute un client dans l'itinéraire                 *)
   (* @Exception: Lance l'exception Failure si le client existe déjà    *)
   (* @Exception: Lance l'exception Failure si la capacité de           *)
   (*             l'itinéraire ne permet pas d'ajouter le client.       *)
    method ajouter_client (c: client) (b: bool) =
      (*initialisation des variables necessaires*)
      let totalDem, capacite, coord, lc = c#get_demande + self#get_demande_totale,
                                          self#get_capacite, c#get_coordonnees, self#get_liste_clients in
      (*gestion des erreurs*)
    	if (totalDem > capacite) then failwith "La capacité de cet itinéraire ne permet pas d'ajouter ce client."
    	else if (self#client_existe c#get_nom) then failwith "Ce client existe déjà."
      (*mise a jours des distances et demandes + fonction de calcul de distance*)
    	else	self#set_demande_totale totalDem; 
  		if (length lc = 0) then c#set_distance (self#calculer_distance coord (0.0,0.0))
  	        else c#set_distance (self#calculer_distance coord self#coord_dernier_client);
                self#set_distance_totale (self#get_distance_totale +. c#get_distance);
                if (b) then self#set_distance_totale (self#get_distance_totale +. self#calculer_distance coord (0.0,0.0));
                (*ajout du client a la liste*)
  		let l = lc@[c] in self#set_liste_clients l
                                       
   (* -- À IMPLANTER (8 PTS) -------------------------------------------*)
   (* @Méthode : supprimer_client : string -> unit                      *)
   (* @Description : Supprime un client d'un itinéraire                 *)
   (* @Exception: Lance l'exception Failure si le client n'existe pas   *)

    method supprimer_client (nomc: string) =
      (*on recupere le client + gere la potentielle erreur*)
      let c = self#retourner_client (nomc) in
          (*mise a jours de demande*)
          self#set_distance_totale (self#get_distance_totale -. c#get_distance);
      (*supression du client*)
      let l = filter (fun x -> ( x#get_nom <> nomc)) self#get_liste_clients in 
      self#set_liste_clients l;
      (*mise a jours des distances des clients*)
      self#maj_distance_clients l;
      (*calcul du nouveau total*)
      let fl = map (fun c -> c#get_distance) l in 
      self#set_distance_totale ((fold_right ( +. ) fl 0.) +. self#calculer_distance self#coord_dernier_client (0.0,0.0))

   (* -- À IMPLANTER (5 PTS) ----------------------------------------------------------*)
   (* @Méthode : ajouter_clients: (string * int * (float * float) * bool) list -> unit *)
   (* @Description : Ajoute plusieurs clients dans l'itinéraire selon                  *)
   (*				 les informations reçues                               *)

    method ajouter_clients (ilc: (string * int * (float * float) * bool) list) =
    	let rec rec_ajouter_client (lnc: (string * int * (float * float) * bool) list) (lc: client list)  = 
    	  match lnc with
          (*on retourne unit si la liste est vide*)
    	  |[] -> ()
          (*sinon on cree un client, l'ajoute a la liste et rappel recusivement*)
    	  |e::r -> let (nom, demande, coord, dernier) = e in
    	    let c = new client nom demande coord in
    	    self#ajouter_client c dernier;
    	    rec_ajouter_client r lc in
    	    rec_ajouter_client ilc self#get_liste_clients

   (* -- À IMPLANTER (5 PTS) ----------------------------------*)
   (* @Méthode : afficher_itineraire : unit                    *)
   (* @Description : Affiche les informations de l'itinéraire  *)

    method afficher_itineraire =
      (*on va chercher le nom de tous les client et les mets dans une liste*)
      let ln = map (fun c -> c#get_nom) self#get_liste_clients in
      (*on transforme cette liste en string*)
      let noms = String.concat " " ln in
      (*on print la string formatee*)
      Printf.printf "DemandeTotale: %d; DistanceTotale: %F; Clients: %s \n" self#get_demande_totale self#get_distance_totale noms

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
      (*on utilise exists pour verifier si l'itineraire se trouve dans la liste*)
      exists (fun i -> i#get_numero = indice) self#get_liste_itineraires

   (* -- À IMPLANTER (4 PTS) -------------------------------------------*)
   (* @Méthode : ajouter_itineraire: itineraire -> unit                 *)
   (* @Description : Ajoute un intinéraire dans le plan                 *)
   (* @Exception: Lance l'exception Failure si l'itinéraire existe déjà *)

    method ajouter_itineraire (i: itineraire) =
      (*lance erreur si l'itineraire est deja dans la liste d'itineraires*)
      if (self#itineraire_existe i#get_numero) then failwith "L'itineraire existe déja."
      (*sinon ajoute l'itineraire a la liste*)
      else self#set_liste_itineraires ((self#get_liste_itineraires)@[i])
    
   (* -- À IMPLANTER (4 PTS) -------------------------------------------*)
   (* @Méthode : retourner_liste_clients: client list                   *)
   (* @Description : Retourne la liste de tous les clients du plan      *)

    method retourner_liste_clients =
      (*equivalent de concat_map, mais n'est pas supporte dans version 4.08*)
      List.concat (map (fun i -> i#get_liste_clients) self#get_liste_itineraires)

   (* -- À IMPLANTER (4 PTS) -------------------------------------------*)
   (* @Méthode : client_existe : string -> bool                         *)
   (* @Description : Détermine si un client existe dans le plan         *)

    method client_existe (nomc: string) =
      exists (fun c -> c#get_nom= nomc) self#retourner_liste_clients

   (* -- À IMPLANTER (5 PTS) ---------------------------------------------------------------------------*)
   (* @Méthode : ajouter_itineraires: int list -> (string * int (float*float) * bool) list list -> unit *)
    (* @Description : Ajoute plusieurs itinéraires dans le plan selon les informations reçues            *)

    method ajouter_itineraires (lcap: int list) (lilc: (string * int * (float*float) * bool) list list) =
      List.iter2 (fun c lc -> let it = new itineraire c in it#ajouter_clients lc;
                  self#ajouter_itineraire it) lcap lilc

   (* -- À IMPLANTER (6 PTS) ------------------------------------------------------*)
   (* @Méthode : retourner_ICRE: (int * int)                                       *)
   (* @Description : Retourne le numéro ainsi que la capacité résiduelle de        *)
   (*                l’itinéraire ayant cette capacité la plus élevée dans le plan *)
   (* @Exception: Lance l'exception Failure si le plan est vide                    *)

    method retourner_ICRE = let l = self#get_liste_itineraires in match l with
    	|[] -> failwith "Le plan est vide."
    	|e::r -> let lp = map (fun i -> (i#get_numero, (i#get_capacite - i#get_demande_totale))) l in
	                let compi i1 i2 = if (snd i1 = snd i2) then 0 else 
	                (if (snd i1 < snd i2) then 1 else -1) in
	                hd (sort compi lp)

   (* -- À IMPLANTER (4 PTS) -------------------------------------------*)
   (* @Méthode : retourner_client : string -> client                    *)
   (* @Description : Détermine si un client existe dans le plan         *)
   (* @Exception: Lance l'exception Failure si le client n'existe pas   *)

    method retourner_client (nomc: string) =
             (*definition de fonction recursive*)
      let rec rec_retourner_client (n: string) (l: client list) =
        match l with
                |[] -> failwith "Ce client n'existe pas."
                |e::r -> if (e#get_nom = n) then e else rec_retourner_client n r in
       (*appel de la fonction recursive avec les bon parametres*)
      rec_retourner_client nomc (self#retourner_liste_clients)

   (* -- À IMPLANTER (4 PTS) --------------------------------------------*)
   (* @Méthode : retourner_itineraire : int -> itineraire                *)
   (* @Description : Retourne l'itinéraire se trouvant dans le plan      *)
   (* @Exception: Lance l'exception Failure si l'itinéraire n'existe pas *)

    method retourner_itineraire (indice: int) =
             (*definition de fonction recursive*)
      let rec rec_retourner_itineraire (n: int) (l: itineraire list) =
        match l with
                |[] -> failwith "L'itinéraire n'existe pas."
                |e::r -> if (e#get_numero = n) then e else rec_retourner_itineraire n r in
       (*appel de la fonction recursive avec les bon parametres*)
      rec_retourner_itineraire indice (self#get_liste_itineraires)

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
