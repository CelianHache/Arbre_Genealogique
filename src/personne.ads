with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

package Personne is

   -- Déclaration du type Personne
   type T_Personne is private;
   type Time_Access is access Ada.Calendar.Time;

   -- Nom: Initialise
   -- Sémantique: Initialise un objet personne avec ses informations
   -- Paramètres:
   --    P :      (out) T_personne - La personne à initialiser
   --    Prenom : (in)  String - Le prenom de la personne
   --    Nom :    (in)  String - Le nom de la personne
   --    Sexe :   (in)  String - Le sexe de la personne
   --    Date_Naissance : (in) Ada.Calendar.Time - La date de naissance de la personne
   --    Lieu_Naissance : (in) String - Le lieu de naissance de la personne
   --    Date_Deces :     (in) Ada.Calendar.Time - La date de decès de la personne
   -- Pré-condition:
   -- Post-Condition:
   procedure Initialise
     (P              : out T_Personne;
      Prenom         : in String;
      Nom            : in String;
      Sexe           : in String;
      Date_Naissance : Time_Access;
      Lieu_Naissance : in String;
      Date_Deces     : Time_Access := null);

   
   -- Nom: Display
   -- Sémantique: Affiche en console une personne
   -- Paramètres:
   --    P : (in) T_personne - La personne à afficher
   -- Pré-condition: 
   -- Post-Condition:
   procedure Display(P : in T_Personne);

   -- Nom: To_String
   -- Sémantique: Retourne sous forme de chaine de caractère une personne
   -- Paramètres:
   --    P : (in) T_personne - La personne
   -- Type retour: String représentant les informations de la personne
   -- Pré-condition: 
   -- Post-Condition:
   function To_String(P : in T_Personne) return String;

   -- Nom: Free_Element
   -- Sémantique: Supprime en mémoire une personne
   -- Paramètres:
   --    P : (in | out) T_personne - La personne à supprimer
   -- Pré-condition: 
   -- Post-Condition:
   procedure Free_Element(P : in out T_Personne);

   -- Nom: Get_Name
   -- Sémantique: Récupérer le nom de la personne
   -- Paramètres:
   --    P : (in) T_personne - La personne
   -- Type retour: String représentant le nom de la personne
   -- Pré-condition: 
   -- Post-Condition:
   function Get_Name(P : in T_Personne) return access String;

   -- Nom: Get_First_Name
   -- Sémantique: Récupérer le prénom de la personne
   -- Paramètres:
   --    P : (in) T_personne - La personne
   -- Type retour: String représentant le prénom de la personne
   -- Pré-condition: 
   -- Post-Condition:
   function Get_First_Name(P : in T_Personne) return access String;

   -- Nom: Get_Gender
   -- Sémantique: Récupérer le sexe de la personne
   -- Paramètres:
   --    P : (in) T_personne - La personne
   -- Type retour: String représentant le sexe de la personne
   -- Pré-condition: 
   -- Post-Condition:
   function Get_Gender(P : in T_Personne) return access String;

   private
      
      type String_Access is access String;

      type T_Personne is record
         Prenom           : String_Access;
         Nom              : String_Access;
         Sexe             : String_Access;
         Date_De_Naissance : Time_Access;
         Lieu_De_Naissance : String_Access;
         Date_De_Deces     : Time_Access := null; -- access pour rendre la date de décès nullable
      end record;

end Personne;