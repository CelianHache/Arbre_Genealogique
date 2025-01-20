with Ada.Text_IO; use Ada.Text_IO;
with Ada.Calendar; use Ada.Calendar;

package Personne is

   -- Déclaration du type Personne
   type T_Personne is private;

   -- Procédures pour gérer les objets Personne

   procedure Initialise
     (P              : out T_Personne;
      Prenom         : in String;
      Nom            : in String;
      Sexe           : in String;
      Date_Naissance : in Ada.Calendar.Time;
      Lieu_Naissance : in String;
      Date_Deces     : access Ada.Calendar.Time := null);

   procedure Display(P : in T_Personne);

   function To_String(P : in T_Personne) return String;

   function Get_Name(P : in T_Personne) return access String;
   function Get_First_Name(P : in T_Personne) return access String;
   function Get_Gender(P : in T_Personne) return access String;

   private
      type T_Personne is record
         Prenom           : access String;
         Nom              : access String;
         Sexe             : access String;
         Date_De_Naissance : Ada.Calendar.Time;
         Lieu_De_Naissance : access String;
         Date_De_Deces     : access Ada.Calendar.Time := null; -- access pour rendre la date de décès nullable
      end record;

end Personne;