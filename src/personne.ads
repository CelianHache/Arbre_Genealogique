with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

package Personne is

   -- Déclaration du type Personne
   type T_Personne is private;
   type Time_Access is access Ada.Calendar.Time;

   -- Procédures pour gérer les objets Personne

   procedure Initialise
     (P              : out T_Personne;
      Prenom         : in String;
      Nom            : in String;
      Sexe           : in String;
      Date_Naissance : Time_Access;
      Lieu_Naissance : in String;
      Date_Deces     : Time_Access := null);

   procedure Display(P : in T_Personne);

   function To_String(P : in T_Personne) return String;

   procedure Free_Element(P : in out T_Personne);
   function Get_Name(P : in T_Personne) return access String;
   function Get_First_Name(P : in T_Personne) return access String;
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