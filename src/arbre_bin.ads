generic
   type Element_Type is private;
   with function To_String ( X : Element_Type) return String;
   with procedure Free_Element(X : in out Element_Type);
package Arbre_Bin is

   type T_Arbre is private;

   -- Nom: Initialise
   -- Sémantique: Initialise un arbre binaire
   -- Paramètres:
   --    Tree :   (in | out) T_Arbre - L'arbre à initialiser
   --    Id   :   (in) String - L'id du noeud racine de cet arbre
   --    Value :  (in) Element_Type - La valeur du noeud racine, type generique
   --    Left_Title :  (in) String - La chaine de caractère qui sera écrite lors de l'affichage de l'enfant gauche de l'arbre
   --    Right_Title : (in) String - La chaine de caractère qui sera écrite lors de l'affichage de l'enfant droit de l'arbre
   -- Pré-condition:
   -- Post-Condition:
   procedure Initialise(Tree : in out T_Arbre; Id :  in String; Value : in Element_Type; Left_Title : in String; Right_Title : in String);

   -- Nom: Add_Right
   -- Sémantique: Ajouter un successeur à droite
   -- Paramètres:
   --    Tree :   (in | out) T_Arbre - L'arbre sur lequel ajouter le successeur
   --    Id   :   (in) String - L'id du successeur qui va être ajouté
   --    Value :  (in) Element_Type - La valeur du successeur
   -- Pré-condition:
   -- Post-Condition:
   procedure Add_Right(Tree : in out T_Arbre; Id :  in String; Value : in Element_Type);

   -- Nom: Add_Left
   -- Sémantique: Ajouter un successeur à gauche
   -- Paramètres:
   --    Tree :   (in | out) T_Arbre - L'arbre sur lequel ajouter le successeur
   --    Id   :   (in) String - L'id du successeur qui va être ajouté
   --    Value :  (in) Element_Type - La valeur du successeur
   -- Pré-condition:
   -- Post-Condition:
   procedure Add_Left(Tree : in out T_Arbre; Id :  in String; Value : in Element_Type);

   -- Nom: Remove_Right
   -- Sémantique: Supprimer le successeur à droite
   -- Paramètres:
   --    Tree :   (in | out) T_Arbre - L'arbre sur lequel supprimer le successeur
   -- Pré-condition:
   -- Post-Condition:
   procedure Remove_Right(Tree : in out T_Arbre);

   -- Nom: Remove_Left
   -- Sémantique: Supprimer le successeur à gauche
   -- Paramètres:
   --    Tree :   (in | out) T_Arbre - L'arbre sur lequel supprimer le successeur
   -- Pré-condition:
   -- Post-Condition:
   procedure Remove_Left(Tree : in out T_Arbre);

   -- Nom: Remove
   -- Sémantique: Supprimer l'arbre
   -- Paramètres:
   --    Tree :   (in | out) T_Arbre - L'arbre a supprimer
   -- Pré-condition:
   -- Post-Condition:
   procedure Remove(Tree : in out T_Arbre);

   -- Nom: Get_Right
   -- Sémantique: Obtenir le successeur droit
   -- Paramètres:
   --    Tree :   (in) T_Arbre - L'arbre sur lequel on accède au successeur droit
   -- Type retour: T_Arbre représentant le successeur droit
   -- Pré-condition:
   -- Post-Condition:
   function Get_Right(Tree : in T_Arbre) return T_Arbre;

   -- Nom: Get_Left
   -- Sémantique: Obtenir le successeur gauche
   -- Paramètres:
   --    Tree :   (in) T_Arbre - L'arbre sur lequel on accède au successeur gauche
   -- Type retour: T_Arbre représentant le successeur gauche
   -- Pré-condition:
   -- Post-Condition:
   function Get_Left(Tree : in T_Arbre) return T_Arbre;

   -- Nom: Get_Id
   -- Sémantique: Obtenir l'identifiant de la racine de l'arbre
   -- Paramètres:
   --    Tree :   (in) T_Arbre - L'arbre sur lequel on accède à l'identifiant
   -- Type retour: String représentant l'identifiant de la racine l'arbre
   -- Pré-condition:
   -- Post-Condition:
   function Get_Id(Tree : in T_Arbre) return String;

   -- Nom: Get_Left_Title
   -- Sémantique: Obtenir le titre pour associé au successeur gauche
   -- Paramètres:
   --    Tree :   (in) T_Arbre - L'arbre sur lequel on accède à l'identifiant
   -- Type retour: String représentant le titre associé au successeur gauche lors de l'affichage de l'arbre
   -- Pré-condition:
   -- Post-Condition:
   function Get_Left_Title(Tree : in T_Arbre) return String;

   -- Nom: Get_Right_Title
   -- Sémantique: Obtenir le titre pour associé au successeur droit
   -- Paramètres:
   --    Tree :   (in) T_Arbre - L'arbre sur lequel on accède à l'identifiant
   -- Type retour: String représentant le titre associé au successeur droit lors de l'affichage de l'arbre
   -- Pré-condition:
   -- Post-Condition:
   function Get_Right_Title(Tree : in T_Arbre) return String;

   -- Nom: Get_Left_Title
   -- Sémantique: Obtenir la valeur de la racine de l'arbre
   -- Paramètres:
   --    Tree :   (in) T_Arbre - L'arbre sur lequel on accède à l'identifiant
   -- Type retour: Element_Type représentant la valeur de la racine de l'arbre
   -- Pré-condition:
   -- Post-Condition:
   function Get_Value(Tree : in T_Arbre) return Element_Type;

   -- Nom: Is_Null
   -- Sémantique: Indiquer si l'arbre est null
   -- Paramètres:
   --    Tree :   (in) T_Arbre - L'arbre sur lequel on accède à l'identifiant
   -- Type retour: Boolean , True si l'arbre n'est pas initialisé, False sinon
   -- Pré-condition:
   -- Post-Condition:
   function Is_Null(Tree : in T_Arbre) return Boolean;

   -- Nom: Display
   -- Sémantique: Affiche en console un arbre bianaire
   -- Paramètres:
   --    Tree : (in) T_Arbre - L'arbre binaire à afficher
   -- Pré-condition: 
   -- Post-Condition:
   procedure Display(Tree : in T_Arbre);

private

   type T_Noeud;
   type T_Arbre is access T_Noeud;
   type String_Access is access String;

   type T_Noeud is
      record
         Id          : String_Access;
         Value       : Element_Type; 
         Right       : T_Arbre;
         Left        : T_Arbre;
         Right_title : String_Access;
         Left_title  : String_Access; 
      end record;


end Arbre_Bin;
