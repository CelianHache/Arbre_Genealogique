generic
   type Element_Type is private;
   with function To_String ( X : Element_Type) return String;
   with procedure Free_Element(X : in out Element_Type);
package Arbre_Bin is

   type T_Arbre is private;

   procedure Initialise(Tree : in out T_Arbre; Id :  in String; Value : in Element_Type; Left_Title : in String; Right_Title : in String);

   -- Ajouter un successeur à droite
   procedure Add_Right(Tree : in out T_Arbre; Id :  in String; Value : in Element_Type);

   -- Ajouter un successeur à gauche
   procedure Add_Left(Tree : in out T_Arbre; Id :  in String; Value : in Element_Type);

   -- Supprimer le successeur droit
   procedure Remove_Right(Tree : in out T_Arbre);

   -- Supprimer le successeur gauche
   procedure Remove_Left(Tree : in out T_Arbre);

   procedure Remove(Tree : in out T_Arbre);

   -- Obtenir le successeur droit
   function Get_Right(Tree : in T_Arbre) return T_Arbre;

   -- Obtenir le successeur gauche
   function Get_Left(Tree : in T_Arbre) return T_Arbre;

   -- Obtenir l'identifiant (ou autre valeur liée au nœud)
   function Get_Id(Tree : in T_Arbre) return String;

   -- Obtenir l'identifiant (ou autre valeur liée au nœud)
   function Get_Left_Title(Tree : in T_Arbre) return String;

   -- Obtenir l'identifiant (ou autre valeur liée au nœud)
   function Get_Right_Title(Tree : in T_Arbre) return String;

   -- Obtenir la valeur contenue dans le nœud
   function Get_Value(Tree : in T_Arbre) return Element_Type;

   function Is_Null(Tree : in T_Arbre) return Boolean;

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
