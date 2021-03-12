(**************************************************************************)
(*                                                                        *)
(*  Admin program for the Major Major mailing list manager                *)
(*  Copyright (C) 2019   Peter Moylan                                     *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU General Public License as published by  *)
(*  the Free Software Foundation, either version 3 of the License, or     *)
(*  (at your option) any later version.                                   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful,       *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of        *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the         *)
(*  GNU General Public License for more details.                          *)
(*                                                                        *)
(*  You should have received a copy of the GNU General Public License     *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>. *)
(*                                                                        *)
(*  To contact author:   http://www.pmoylan.org   peter@pmoylan.org       *)
(*                                                                        *)
(**************************************************************************)

IMPLEMENTATION MODULE CommonSettings;

        (****************************************************************)
        (*                                                              *)
        (*                  Admin program for MajorMajor                *)
        (*             Data common to the main Admin notebook           *)
        (*                                                              *)
        (*    Started:        22 October 2003                           *)
        (*    Last edited:    29 September 2019                         *)
        (*    Status:         OK                                        *)
        (*                                                              *)
        (****************************************************************)


IMPORT OS2, Strings, FileSys, INIData, RINIData, Languages;

FROM Names IMPORT
    (* type *)  FilenameString;

(************************************************************************)

CONST
    Nul = CHR(0);

TYPE
    FontNameArray = ARRAY FontGroup OF FontName;

<* PUSH *>
<* VOLATILE+ *>

VAR
    INIFileName: FilenameString;

    OurFontName: FontNameArray;
    OurLanguage: Languages.LangHandle;
    LanguageCode: ARRAY [0..31] OF CHAR;
    AdminINI: ARRAY [0..9] OF CHAR;

<* POP *>

CONST
    FontGroupLabel = FontNameArray {"notebook", "ListNotebook"};
    DefaultFont = FontNameArray {"8.Helv", "8.Helv"};

(************************************************************************)

PROCEDURE CurrentFont (group: FontGroup;  VAR (*OUT*) fontname: FontName);

    (* Returns the currently set font for this group. *)

    BEGIN
        fontname := OurFontName[group];
    END CurrentFont;

(************************************************************************)

PROCEDURE UpdateFontFrom (hwnd: OS2.HWND;  group: FontGroup);

    (* Takes the font setting from window hwnd and propagates it to the *)
    (* entire group.  Note that this will often be a "no operation"     *)
    (* because the font is no different from the presently active one.  *)

    VAR NewFontName: FontName;
        AttrFound, length: CARDINAL;
        hini: INIData.HINI;  target: OS2.HWND;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        length := OS2.WinQueryPresParam (hwnd, OS2.PP_FONTNAMESIZE, 0,
                                     AttrFound, FontNameSize, NewFontName,
                                      0(*OS2.QPF_NOINHERIT*));
        IF length < FontNameSize THEN
            NewFontName[length] := Nul;
        END (*IF*);

        IF NOT Strings.Equal (NewFontName, OurFontName[group]) THEN

            OurFontName[group] := NewFontName;
            hini := INIData.OpenINIFile (AdminINI);
            app := "Font";
            INIData.INIPutString (hini, app, FontGroupLabel[group],
                                                        OurFontName[group]);
            INIData.CloseINIFile (hini);

            (* For reasons that are still a mystery to me, we have to go    *)
            (* up three levels in the hierarchy to get from here to the     *)
            (* frame.  (I calculated two, but there's apparently an extra   *)
            (* container window that we're not told about.)                 *)

            target := OS2.WinQueryWindow(hwnd, OS2.QW_PARENT);
            target := OS2.WinQueryWindow(target, OS2.QW_PARENT);
            target := OS2.WinQueryWindow(target, OS2.QW_PARENT);
            OS2.WinSendMsg (target, FONTCHANGED, NIL, NIL);

        END (*IF*);

    END UpdateFontFrom;

(************************************************************************)

PROCEDURE ChangeLanguageTo (name: ARRAY OF CHAR): BOOLEAN;

    (* Changes the current language setting, but only if a suitable     *)
    (* Admin.name.lng file exists.  Returns FALSE if no change.         *)

    CONST prefix = "Admin";

    VAR result: BOOLEAN;  file: FilenameString;

    BEGIN
        Strings.Capitalize (name);
        result := FALSE;
        IF NOT Strings.Equal (name, LanguageCode) THEN
            Strings.Assign (prefix, file);
            Strings.Append (".", file);
            Strings.Append (name, file);
            Strings.Append (".lng", file);
            IF FileSys.Exists (file) THEN
                Languages.DropLanguage (OurLanguage);
                Strings.Assign (name, LanguageCode);
                OurLanguage := Languages.UseLanguage (prefix, name);
                result := TRUE;
            END (*IF*);
        END (*IF*);
        RETURN result;
    END ChangeLanguageTo;

(************************************************************************)

PROCEDURE CurrentLanguage (VAR (*OUT*) lang: Languages.LangHandle;
                                VAR (*OUT*) name: ARRAY OF CHAR);

    (* Returns the current language setting. *)

    BEGIN
        lang := OurLanguage;
        Strings.Assign (LanguageCode, name);
    END CurrentLanguage;

(************************************************************************)

PROCEDURE SetInitialLanguage;

    (* Sets the language from the Major Major INI file. *)

    BEGIN
        IF RINIData.OpenINIFile(INIFileName) THEN
            IF NOT RINIData.INIGetString ('$SYS', 'AdminLanguage', LanguageCode)
                                         OR (LanguageCode[0] = Nul) THEN
                LanguageCode := "en";
            END (*IF*);
            RINIData.CloseINIFile;
        ELSE
            LanguageCode := "en";
        END (*IF*);
        OurLanguage := Languages.UseLanguage ("Admin", LanguageCode);
    END SetInitialLanguage;

(**************************************************************************)

PROCEDURE SetInitialFontsAndLanguage;

    (* Initialisation for CommonSettings module. *)

    VAR hini: INIData.HINI;  group: FontGroup;
        app: ARRAY [0..4] OF CHAR;

    BEGIN
        hini := INIData.OpenINIFile(AdminINI);
        IF NOT INIData.INIValid(hini) THEN
            hini := INIData.CreateINIFile(AdminINI);
        END (*IF*);
        app := "Font";
        FOR group := MIN(FontGroup) TO MAX(FontGroup) DO
            IF NOT INIData.INIGetString (hini, app, FontGroupLabel[group],
                                                OurFontName[group])
                          OR (OurFontName[group][0] = Nul) THEN
                OurFontName[group] := DefaultFont[group];
                INIData.INIPutString (hini, app, FontGroupLabel[group],
                                                    OurFontName[group]);
            END (*IF*);
        END (*FOR*);
        INIData.CloseINIFile (hini);
        SetInitialLanguage;
    END SetInitialFontsAndLanguage;

(************************************************************************)

PROCEDURE SetINIFileName (name: ARRAY OF CHAR);

    (* Sets the INI file name and mode. *)

    VAR pos: CARDINAL;  found: BOOLEAN;

    BEGIN
        Strings.Assign (name, INIFileName);
        Strings.FindPrev ('.', name, LENGTH(name)-1, found, pos);
        Strings.Delete (name, 0, pos);
        Strings.Insert ("Admin", 0, name);
        Strings.Assign (name, AdminINI);
        SetInitialFontsAndLanguage;
    END SetINIFileName;

(**************************************************************************)

BEGIN
    AdminINI := "Admin.INI";
END CommonSettings.

