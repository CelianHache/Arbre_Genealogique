with Arbre_Bin;
with Personne;

package Arbre_Genealog is

   package Arbre_Personnes is new Arbre_Bin (
      Personne.T_Personne,
      Personne.To_String,
      Personne.Free_Element
   );
   use Arbre_Personnes;

   type T_Arbre_Personnes is new T_Arbre;
   type Ancestor_Array is array (Positive range <>) of Personne.T_Personne;  

   -- Nom : Create_Family_Tree
   -- Sémantique : Crée un arbre généalogique en initialisant sa racine avec une personne donnée.
   -- Paramètres :
   --    Root :       (in out) T_Arbre_Personnes - L'arbre généalogique à initialiser.
   --    Root_Value : (in)     Personne.T_Personne - La personne représentant la racine de l'arbre.
   -- Pré-condition :
   --    - Root doit être non initialisé ou vide avant l'appel de la procédure.
   -- Post-condition :
   --    - La racine de l'arbre contient la personne spécifiée.
   procedure Create_Family_Tree(Root : in out T_Arbre_Personnes; Root_Value : Personne.T_Personne);

   -- Nom : Display_Family_Tree
   -- Sémantique : Affiche l'arbre généalogique complet dans la console.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique à afficher.
   -- Pré-condition :
   --    - L'arbre doit être initialisé.
   -- Post-condition :
   --    - Les informations de toutes les personnes de l'arbre sont affichées dans la console.
   procedure Display_Family_Tree(Tree : in T_Arbre_Personnes);

   -- Nom : Display_Family_Tree_From_Node
   -- Sémantique : Affiche l'arbre généalogique à partir d'un nœud donné dans la console.
   -- Paramètres :
   --    Tree :    (in) T_Arbre_Personnes - L'arbre généalogique.
   --    Id_Node : (in) String - L'identifiant du nœud à partir duquel afficher l'arbre.
   -- Pré-condition :
   --    - Le nœud spécifié par Id_Node existe dans l'arbre.
   -- Post-condition :
   --    - Les informations des personnes descendant du nœud spécifié sont affichées dans la console.
   procedure Display_Family_Tree_From_Node(Tree : in T_Arbre_Personnes; Id_Node: String);

   -- Nom : Get_Father
   -- Sémantique : Retourne le sous-arbre correspondant au père du nœud courant.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   -- Type retour :
   --    T_Arbre_Personnes - Le sous-arbre représentant le père.
   -- Pré-condition :
   --    - Le nœud courant a un père dans l'arbre.
   -- Post-condition :
   --    - Le sous-arbre retourné correspond au père du nœud courant.
   function Get_Father (Tree : in T_Arbre_Personnes) return T_Arbre_Personnes;

   -- Nom : Get_Mother
   -- Sémantique : Retourne le sous-arbre correspondant à la mère du nœud courant.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   -- Type retour :
   --    T_Arbre_Personnes - Le sous-arbre représentant la mère.
   -- Pré-condition :
   --    - Le nœud courant a une mère dans l'arbre.
   -- Post-condition :
   --    - Le sous-arbre retourné correspond à la mère du nœud courant.
   function Get_Mother (Tree : in T_Arbre_Personnes) return T_Arbre_Personnes;

   -- Nom : Add_Father
   -- Sémantique : Ajoute un père à un nœud dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in out) T_Arbre_Personnes - L'arbre généalogique auquel ajouter le père.
   --    Value : (in) Personne.T_Personne - La personne représentant le père à ajouter.
   -- Pré-condition :
   --    - Le nœud courant ne doit pas avoir de père déjà ajouté.
   -- Post-condition :
   --    - Le père est ajouté au nœud spécifié.
   procedure Add_Father (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne);

   -- Nom : Add_Mother
   -- Sémantique : Ajoute une mère à un nœud dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in out) T_Arbre_Personnes - L'arbre généalogique auquel ajouter la mère.
   --    Value : (in) Personne.T_Personne - La personne représentant la mère à ajouter.
   -- Pré-condition :
   --    - Le nœud courant ne doit pas avoir de mère déjà ajoutée.
   -- Post-condition :
   --    - La mère est ajoutée au nœud spécifié.
   procedure Add_Mother (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne);

   -- Nom : Add_Father
   -- Sémantique : Ajoute un père à un nœud spécifié par l'identifiant de l'enfant dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in out) T_Arbre_Personnes - L'arbre généalogique auquel ajouter le père.
   --    Value : (in) Personne.T_Personne - La personne représentant le père à ajouter.
   --    Id_Child : (in) String - L'identifiant de l'enfant à qui ajouter le père.
   -- Pré-condition :
   --    - L'enfant identifié par Id_Child existe dans l'arbre.
   -- Post-condition :
   --    - Le père est ajouté à l'enfant spécifié.
   procedure Add_Father (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne; Id_Child: String);

   -- Nom : Add_Mother
   -- Sémantique : Ajoute une mère à un nœud spécifié par l'identifiant de l'enfant dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in out) T_Arbre_Personnes - L'arbre généalogique auquel ajouter la mère.
   --    Value : (in) Personne.T_Personne - La personne représentant la mère à ajouter.
   --    Id_Child : (in) String - L'identifiant de l'enfant à qui ajouter la mère.
   -- Pré-condition :
   --    - L'enfant identifié par Id_Child existe dans l'arbre.
   -- Post-condition :
   --    - La mère est ajoutée à l'enfant spécifié.
   procedure Add_Mother (Tree : in out T_Arbre_Personnes; Value: Personne.T_Personne; Id_Child: String);

   -- Nom : Get_Node_By_Id
   -- Sémantique : Récupère un nœud dans l'arbre généalogique en utilisant l'identifiant d'un enfant.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   --    Id_Child : (in) String - L'identifiant de l'enfant à retrouver.
   -- Type retour :
   --    T_Arbre_Personnes - Le nœud correspondant à l'identifiant de l'enfant.
   -- Pré-condition :
   --    - L'identifiant d'un enfant existe dans l'arbre.
   -- Post-condition :
   function Get_Node_By_Id (Tree : in T_Arbre_Personnes; Id_Child : String) return T_Arbre_Personnes;

   -- Nom : Get_Ancestors_Generation
   -- Sémantique : Récupère les ancêtres d'une génération spécifique dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   --    Generation : (in) Integer - Le numéro de la génération à récupérer.
   -- Type retour :
   --    Ancestor_Array - Tableau des ancêtres appartenant à la génération spécifiée.
   -- Pré-condition :
   --    - La génération spécifiée doit exister dans l'arbre.
   -- Post-condition :
   function Get_Ancestors_Generation (Tree : in T_Arbre_Personnes; Generation: Integer) return Ancestor_Array;

   -- Nom : Get_Sorted_Ancestor_Array
   -- Sémantique : Trie un tableau d'ancêtres par ordre d'apparition.
   -- Paramètres :
   --    List : (in) Ancestor_Array - Le tableau des ancêtres à trier.
   -- Type retour :
   --    Ancestor_Array - Le tableau des ancêtres trié.
   -- Pré-condition :
   --    - Le tableau des ancêtres ne doit pas être vide.
   -- Post-condition :
   --    - Le tableau des ancêtres est trié.
   function Get_Sorted_Ancestor_Array(List : in Ancestor_Array) return Ancestor_Array;

   -- Nom : Equals
   -- Sémantique : Vérifie si deux tableaux d'ancêtres sont identiques (c'est-à-dire s'ils contiennent les mêmes éléments dans le même ordre).
   -- Paramètres :
   --    Array1 : (in) Ancestor_Array - Le premier tableau d'ancêtres à comparer.
   --    Array2 : (in) Ancestor_Array - Le second tableau d'ancêtres à comparer.
   -- Type retour :
   --    Boolean - True si les deux tableaux sont égaux, sinon False.
   -- Pré-condition :
   --    - Les deux tableaux doivent être valides.
   -- Post-condition :
   function Equals(Array1 : in Ancestor_Array; Array2 : in Ancestor_Array) return boolean;
   
   -- Nom : Count_Ancestors
   -- Sémantique : Compte le nombre d'ancêtres d'un nœud spécifié dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   --    Id_Node : (in) String - L'identifiant du nœud pour lequel compter les ancêtres.
   -- Type retour :
   --    Integer - Le nombre d'ancêtres du nœud spécifié.
   -- Pré-condition :
   --    - Le nœud identifié par Id_Node doit exister dans l'arbre.
   -- Post-condition :
   function Count_Ancestors(Tree: in T_Arbre_Personnes; Id_Node : in String) return Integer;

   -- Nom : Remove_Family_Member
   -- Sémantique : Supprime un membre de la famille spécifié par son identifiant de l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in | out) T_Arbre_Personnes - L'arbre généalogique.
   --    Id_Node : (in) String - L'identifiant du membre à supprimer.
   -- Pré-condition :
   --    - Le membre spécifié par Id_Node doit exister dans l'arbre.
   -- Post-condition :
   --    - Le membre identifié est supprimé de l'arbre.
   procedure Remove_Family_Member(Tree: in out T_Arbre_Personnes; Id_Node: in String);

   -- Nom : Remove_Father
   -- Sémantique : Supprime le père du nœud spécifié de l'arbre généalogique.
   -- Paramètres :
   --    Child : (in | out) T_Arbre_Personnes - Le nœud enfant dont le père doit être supprimé.
   -- Pré-condition :
   --    - Le nœud enfant doit avoir un père dans l'arbre.
   -- Post-condition :
   --    - Le père du nœud enfant est supprimé de l'arbre.
   procedure Remove_Father(Child: in out T_Arbre_Personnes);

   -- Nom : Remove_Mother
   -- Sémantique : Supprime la mère du nœud spécifié de l'arbre généalogique.
   -- Paramètres :
   --    Child : (in | out) T_Arbre_Personnes - Le nœud enfant dont la mère doit être supprimée.
   -- Pré-condition :
   --    - Le nœud enfant doit avoir une mère dans l'arbre.
   -- Post-condition :
   --    - La mère du nœud enfant est supprimée de l'arbre.
   procedure Remove_Mother(Child: in out T_Arbre_Personnes);

   -- Nom : Get_Child
   -- Sémantique : Récupère un enfant d'un nœud spécifié dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   --    Id_Node : (in) String - L'identifiant du nœud pour lequel récupérer l'enfant.
   -- Type retour :
   --    T_Arbre_Personnes - L'arbre généalogique représentant l'enfant du nœud spécifié.
   -- Pré-condition :
   --    - L'identifiant Id_Node doit être valide et correspondre à un nœud existant dans l'arbre.
   -- Post-condition :
   function Get_Child(Tree: in T_Arbre_Personnes; Id_Node : in String) return T_Arbre_Personnes with
      Pre => Id_Node'Length > 1;

   -- Nom : Is_Father
   -- Sémantique : Vérifie si le nœud spécifié est un père dans l'arbre généalogique.
   -- Paramètres :
   --    Id_Node : (in) String - L'identifiant du nœud à vérifier.
   -- Type retour :
   --    Boolean - Retourne True si le nœud est un père, sinon False.
   -- Pré-condition :
   --    - L'identifiant Id_Node doit être valide et correspondre à un nœud existant dans l'arbre.
   -- Post-condition :
   function Is_Father(Id_Node: in String) return Boolean;

   -- Nom : Nodes_With_Two_Parents
   -- Sémantique : Récupère tous les nœuds ayant deux parents dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   -- Type retour :
   --    Ancestor_Array - Un tableau contenant les nœuds ayant deux parents.
   -- Pré-condition :
   --    - L'arbre généalogique doit être valide.
   -- Post-condition :
   function Nodes_With_Two_Parents(Tree : in T_Arbre_Personnes) return Ancestor_Array;

   -- Nom : Nodes_With_Only_One_Parent
   -- Sémantique : Récupère tous les nœuds ayant un seul parent dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   -- Type retour :
   --    Ancestor_Array - Un tableau contenant les nœuds ayant un seul parent.
   -- Pré-condition :
   --    - L'arbre généalogique doit être valide.
   -- Post-condition :
   function Nodes_With_Only_One_Parent(Tree : in T_Arbre_Personnes) return Ancestor_Array;

   -- Nom : Nodes_Without_Parent
   -- Sémantique : Récupère tous les nœuds sans parent dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   -- Type retour :
   --    Ancestor_Array - Un tableau contenant les nœuds sans parent.
   -- Pré-condition :
   --    - L'arbre généalogique doit être valide.
   -- Post-condition :
   function Nodes_Without_Parent(Tree : in T_Arbre_Personnes) return Ancestor_Array;

   -- Nom : Has_Two_Parents
   -- Sémantique : Vérifie si le nœud spécifié a deux parents dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   -- Type retour :
   --    Boolean - Retourne True si le nœud spécifié a deux parents, sinon False.
   -- Pré-condition :
   --    - L'arbre généalogique doit être valide.
   -- Post-condition :
   function Has_Two_Parents(Tree : in T_Arbre_Personnes) return boolean;

   -- Nom : Has_Only_One_Parent
   -- Sémantique : Vérifie si le nœud spécifié a seulement un parent dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   -- Type retour :
   --    Boolean - Retourne True si le nœud spécifié a un seul parent, sinon False.
   -- Pré-condition :
   --    - L'arbre généalogique doit être valide.
   -- Post-condition :
   function Has_Only_One_Parent(Tree : in T_Arbre_Personnes) return boolean;

   -- Nom : Is_Orphan
   -- Sémantique : Vérifie si le nœud spécifié est un orphelin dans l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   -- Type retour :
   --    Boolean - Retourne True si le nœud spécifié est un orphelin, sinon False.
   -- Pré-condition :
   --    - L'arbre généalogique doit être valide.
   -- Post-condition :
   function Is_Orphan(Tree : in T_Arbre_Personnes) return boolean;

   -- Nom : Get_Tree_Depth
   -- Sémantique : Récupère la profondeur de l'arbre généalogique.
   -- Paramètres :
   --    Tree : (in) T_Arbre_Personnes - L'arbre généalogique.
   -- Type retour :
   --    Integer - La profondeur de l'arbre généalogique.
   -- Pré-condition :
   --    - L'arbre généalogique doit être valide.
   -- Post-condition :
   function Get_Tree_Depth(Tree: in T_Arbre_Personnes) return Integer;

end Arbre_Genealog;
