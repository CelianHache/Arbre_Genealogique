generic
   type Element_Type is private;
   type Id_Type is private;
   with function To_String ( X : Element_Type) return String;
   with function To_String ( X : Id_Type) return String;
package Arbre_Bin is

   type T_Arbre is limited private;

   procedure Initialise(Tree : in out T_Arbre; Id :  in Id_Type; Value : in Element_Type);

   -- Ajouter un successeur à droite
   procedure Add_Right(Tree : in out T_Arbre; Id :  in Id_Type; Value : in Element_Type);

   -- Ajouter un successeur à gauche
   procedure Add_Left(Tree : in out T_Arbre; Id :  in Id_Type; Value : in Element_Type);

   -- Supprimer le successeur droit
   procedure Remove_Right(Tree : in out T_Arbre);

   -- Supprimer le successeur gauche
   procedure Remove_Left(Tree : in out T_Arbre);

   -- Obtenir le successeur droit
   function Get_Right(Tree : in T_Arbre) return T_Arbre;

   -- Obtenir le successeur gauche
   function Get_Left(Tree : in T_Arbre) return T_Arbre;

   -- Obtenir l'identifiant (ou autre valeur liée au nœud)
   function Get_Id(Tree : in T_Arbre) return Id_Type;

   -- Obtenir la valeur contenue dans le nœud
   function Get_Value(Tree : in T_Arbre) return Element_Type;

   function Is_Null(Tree : in T_Arbre) return Boolean;

   procedure Display(Tree : in T_Arbre);

private

   type T_Noeud;
   type T_Arbre is access T_Noeud;

   type T_Noeud is
      record
         Id     : Id_Type;
         Value  : Element_Type; 
         Right  : T_Arbre;
         Left   : T_Arbre;
      end record;


end Arbre_Bin;