{
  AnyConditionsMatchReplace v2.0.0
  created by COACHWICKWACK
  
  *CHANGES*
  v2.0.0
    - Updated for xEdit 4.1.x
  v1.2.0
    - ElementDataReplacer -> AnyConditionsMatchReplace
    - Cleaned up code
  v1.1.0
    - Moved core functions and procedures to ASSLib
    - Fixed bug with indexing indexes in structs
  v1.0.0
    - Initial implementation
  
  This script can be used to replace any element values matched for selected records.
}

Unit AnyConditionsMatchReplace;

// Include the xEdit scripting framework

Uses xEditAPI, Classes, SysUtils, ASSLib;

Const 
  // sTypes = 'string'#13'integer'#13'int64'#13'cardinal'#13'array';
  vs = 'v2';
  editvalues = true;
  // don't change this unless you know what you're doing
  nativevalues = false;
  // importing only supports edit values
  panelwidth = 800;
  plusminuswidth = 55;
  plusminusgap = 5;
  generalgap = 8;
  rowwidth = panelwidth - generalgap*6;
  buttonswidth = 166;

Var 
  i, j: integer;
  path, find, target, replace: String;
  outputPath: String;
  eJson: Boolean;
  frm: TForm;
  pnlBottom: TPanel;
  btnOk, btnCancel, btnPlus, btnMinus: TButton;
  cb1, cb2: TCheckBox;
  lbl1, lbl2, leftlbl, middlelbl, rightlbl, replacelbl: TLabel;
  slPaths, slMatches, slTargets, slReplaces: TStringList;
  lstPath, lstMatch, lstTarget, lstReplace: TList;
  jsonObj: TJsonObject;

Procedure AddElementToJson(sPathList: TStringList; Obj: TJsonObject; filename:
                           String; category: String; EDID: String; elementValue:
                           String);

Var 
  i: integer;
  ChildObj: TJsonObject;
Begin
  // Begin building json with filename and category of record
  ChildObj := Obj;
  ChildObj := ChildObj.O[filename];
  ChildObj := ChildObj.O[category];
  ChildObj := ChildObj.O[EDID];

  If length(sPathList) = 1 Then Begin
    Obj.S[sPathList[0]] = elementValue;
  End Else Begin
    For i := 0 To Pred(sPathList.Count) - 1 Do Begin
      If IsIndex(sPathList[i]) Then Begin
        Try
          ChildObj := ChildObj.O[ExtractIndexStr(sPathList[i])];
        Except
          ChildObj := ChildObj.AddObject;
        End
      End Else If IsIndex(sPathList[i+1]) Then Begin
        ChildObj := ChildObj.A[sPathList[i]];
      End Else If Not IsIndex(sPathList[i]) Then Begin
        ChildObj := ChildObj.O[sPathList[i]];
      End;
    End;

  // If index is last index:
  ChildObj.S[sPathList[Pred(sPathList.Count)]] := elementValue;
  End;
End;

//=========================================================================
// AddPathEntry: Creates a new path entry
Procedure AddPathEntry;

Var 
  ed, em, et, er: TEdit;
Begin
  ed := TEdit.Create(frm);
  // Query Path
  ed.Parent := frm;
  ed.Left := 8;
  ed.Top := 50 + (30 * lstPath.Count);
  ed.Width := rowwidth*0.3;

  em := TEdit.Create(frm);
  // Regex Query
  em.Parent := frm;
  em.Left := ed.Left + ed.Width + 8;
  em.Top := 50 + (30 * lstPath.Count);
  em.Width := rowwidth*0.2;

  et := TEdit.Create(frm);
  // Target Path
  et.Parent := frm;
  et.Left := em.Left + em.Width + 8;
  et.Top := 50 + (30 * lstPath.Count);
  et.Width := rowwidth*0.3;

  er := TEdit.Create(frm);
  // Replace value
  er.Parent := frm;
  er.Left := et.Left + et.Width + 8;
  er.Top := 50 + (30 * lstPath.Count);
  er.Width := rowwidth*0.2;

  // Add entries
  lstPath.Add(ed);
  lstMatch.Add(em);
  lstTarget.Add(et);
  lstReplace.Add(er);
End;

//=========================================================================
// DelPathEntry: Deletes the bottom path entry
Procedure DelPathEntry;
Begin
  If lstPath.Count > 0 Then Begin
    TEdit(lstPath[Pred(lstPath.Count)]).Free;
    TEdit(lstMatch[Pred(lstMatch.Count)]).Free;
    TEdit(lstTarget[Pred(lstTarget.Count)]).Free;
    TEdit(lstReplace[Pred(lstReplace.Count)]).Free;

    lstPath.Delete(Pred(lstPath.Count));
    lstMatch.Delete(Pred(lstMatch.Count));
    lstTarget.Delete(Pred(lstTarget.Count));
    lstReplace.Delete(Pred(lstReplace.Count));
  End;
End;

//=========================================================================
// PathManager: Adds or deletes path entries
Procedure frm.PathManager(Sender: TObject);
Begin
  If (Sender = btnPlus) Then Begin
    AddPathEntry;
    frm.Height := 240 + 30*(lstPath.Count);
  End;
  If (Sender = btnMinus) And (lstPath.Count > 1) Then Begin
    DelPathEntry;
    frm.Height := 240 + 30*(lstPath.Count);
  End;
End;

{
  Procedure for prompting user options
}
Procedure OptionsForm;
Begin
  frm := TForm.Create(Nil);
  Try
    frm.Caption := 'AnyConditionsMatchReplace ' + vs;
    frm.Width := panelwidth;
    frm.Height := 270;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;

    pnlBottom := TPanel.Create(frm);
    pnlBottom.Parent := frm;
    pnlBottom.BevelOuter := bvNone;
    pnlBottom.Align := alBottom;
    pnlBottom.Height := 190;

    btnPlus := TButton.Create(frm);
    btnPlus.Parent := pnlBottom;
    btnPlus.Caption := '+';
    btnPlus.Width := (plusminuswidth - plusminusgap)/2;
    btnPlus.Left := (panelwidth - plusminuswidth)/2;
    btnPlus.Top := 35;
    btnPlus.OnClick := PathManager;

    btnMinus := TButton.Create(frm);
    btnMinus.Parent := pnlBottom;
    btnMinus.Caption := '-';
    btnMinus.Width := (plusminuswidth - plusminusgap)/2;
    btnMinus.Left := btnPlus.Left + btnMinus.Width + 5;
    btnMinus.Top := 35;
    btnMinus.OnClick := PathManager;

    lbl1 := TLabel.Create(frm);
    lbl1.Parent := frm;
    lbl1.Top := 0;
    lbl1.Left := 8;
    lbl1.Width := 360;
    lbl1.Height := 50;
    lbl1.Caption := 'Enter the paths you want to find and replace values ';

    lbl2 := TLabel.Create(frm);
    lbl2.Parent := pnlBottom;
    lbl2.Top := 65;
    lbl2.Left := 8;
    lbl2.AutoSize := False;
    lbl2.Wordwrap := True;
    lbl2.Width := 360;
    lbl2.Caption := 'Export options:';

    leftlbl := TLabel.Create(frm);
    leftlbl.Parent := frm;
    leftlbl.Top := 30;
    leftlbl.Left := 8;
    leftlbl.AutoSize := False;
    leftlbl.Wordwrap := True;
    leftlbl.Width := (panelwidth - 40) / 4;
    leftlbl.Caption := 'Path Query';

    middlelbl := TLabel.Create(frm);
    middlelbl.Parent := frm;
    middlelbl.Top := 30;
    middlelbl.Left := 16 + rowwidth*0.3;
    middlelbl.AutoSize := False;
    middlelbl.Wordwrap := True;
    middlelbl.Width := (panelwidth - 40) / 4;
    middlelbl.Caption := 'Match Regex';

    rightlbl := TLabel.Create(frm);
    rightlbl.Parent := frm;
    rightlbl.Top := 30;
    rightlbl.Left := 8 + middlelbl.Left + rowwidth*0.2;
    rightlbl.AutoSize := False;
    rightlbl.Wordwrap := True;
    rightlbl.Width := (panelwidth - 40) / 4;
    rightlbl.Caption := 'Path Target';

    replacelbl := TLabel.Create(frm);
    replacelbl.Parent := frm;
    replacelbl.Top := 30;
    replacelbl.Left := panelwidth + 24 - rightlbl.Width;
    replacelbl.AutoSize := False;
    replacelbl.Wordwrap := True;
    replacelbl.Width := (panelwidth - 40) / 4;
    replacelbl.Caption := 'Replace Value';

    cb1 := TCheckBox.Create(frm);
    cb1.Parent := pnlBottom;
    cb1.Top := 90;
    cb1.Left := 8;
    cb1.Width := 150;
    cb1.Caption := ' Export as .json';

    btnOk := TButton.Create(frm);
    btnOk.Parent := pnlBottom;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := (panelwidth - buttonswidth)/2;
    btnOk.Top := 150;

    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnlBottom;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + 16;
    btnCancel.Top := 150;

    // Spawn 3 rows of Entry Rows
    // for i := 0 to 2 do
    AddPathEntry;
    // Spawn 1 row for entry

    If frm.ShowModal = mrOk Then Begin
      For i := 0 To Pred(lstPath.Count) Do Begin
        If SameText(TEdit(lstPath[i]).Text, '') Then Continue;

        slPaths.Add(TEdit(lstPath[i]).Text);
        slMatches.Add(TEdit(lstMatch[i]).Text);
        slTargets.Add(TEdit(lstTarget[i]).Text);
        slReplaces.Add(TEdit(lstReplace[i]).Text);
      End;
      If cb1.Checked = cbChecked Then eJson := true;
    End;
  Finally
    frm.Free;
  End;
End;

Function Initialize: integer;
Begin
  // Welcome messages
  AddMessage(
'-------------------------------------------------------------------------------'
  );

  // Object creation
  lstPath := TList.Create;
  lstMatch := TList.Create;
  lstTarget := TList.Create;
  lstReplace := TList.Create;

  slPaths := TStringList.Create;
  // Query Paths
  slMatches := TStringList.Create;
  // Regex Matches
  slTargets := TStringList.Create;
  // Target element paths
  slReplaces := TStringList.Create;
  // Replace values

  jsonObj := TJsonObject.Create;
  AddMessage('Objects created.'+#13#10);

  // options form
  OptionsForm;
  // Invalid if check below... Can search for empty paths
  If slPaths.Count > 0 Then
    AddMessage(#13 + 'Processing records...')
  Else
    AddMessage('No paths specified, terminating script.');
End;

Procedure ReplaceElement(replace: String; element: IInterface);
Var 
  container: IInterface;
Begin
  Try
    SetEditValue(element, replace);
    // Attempt simple replace
  Except
    Try
      SetNativeValue(element, replace);
    Except
      AddMessage('Failed to replace ' + MyIndexedPath(element))
    End;
  End;
End;

Procedure CheckTarget(val: String; target: String; filename: String; category:
                      String; recordEditorId: String; replace: String; e:
                      IInterface; currentIndexedPath: String; spl: TStringList;
                      element: IInterface);
Begin
  If target <> '' Then Begin
    ProcessTarget(filename, category, recordEditorId, target, replace, e);
  End
  Else If target = '' Then Begin
    AddMessage('	Target "' + currentIndexedPath +'" = ' + val + '-> ' + replace);

    ReplaceElement(replace, element);
    If eJson Then Begin
      spl := SplitStrToListFunc(currentIndexedPath, '\', spl);
      AddElementToJson(spl, jsonObj, filename, category, recordEditorId, val);
    End;
  End;
End;

Procedure ProcessTarget(filename: String; category: String; recordEditorId:
                        String; target: String; replace: String; e: IInterface);
Var 
  tsl, tspl: TStringList;
  tlst: TList;
  j: integer;
  val, currentIndexedPath: string;
  element: IInterface;
Begin
  AddMessage('Processing target for '+ Name(e));
  // Remove trailing slash
  If Pos('\', target) = Length(target) Then target := Copy(target, 1, Length(target) - 1);

  // create lists for target path
  tlst := TList.Create;
  tsl := TStringList.Create;
  // Elements matched
  tspl := TStringList.Create;
  // Split indexed path
  ElementsByMIP(tlst, e, target);
  mgeev(tsl, tlst);

  // match find
  For j := 0 To Pred(tsl.Count) Do Begin
    element := ObjectToElement(tlst[j]);
    If Assigned(element) Then Begin
      val := tsl[j];
      currentIndexedPath := MyIndexedPath(element);
      AddMessage('	Target "' + currentIndexedPath +'" = ' + val + '-> ' + replace);

      ReplaceElement(replace, element);
      If eJson Then Begin
        tspl := SplitStrToListFunc(currentIndexedPath, '\', tspl);
        AddElementToJson(tspl, jsonObj, filename, category, recordEditorId, replace);
      End;
    End;
  End;
  // free memory
  tlst.Free;
  tsl.Free;
  tspl.Free;
End;

Function Process(e: IInterface): integer;
Var 
  lst, tlst: TList;
  sl, spl, tsl, tspl: TStringList;
  i, j: integer;
  val, filename, category, recordEditorId, currentIndexedPath: string;
  element: IInterface;
Begin
  // Build json like {filename: {category: {recordEditorId: {IndexedPath}}}}
  filename := BaseName(GetFile(e));
  category := Signature(e);
  recordEditorId := EditorID(e);

  If slPaths.Count = 0 Then exit;
  For i := 0 To Pred(slPaths.Count) Do Begin
    path := slPaths[i];
    // Query Path
    find := slMatches[i];
    // Regex Match
    target := slTargets[i];
    // Target path
    replace := slReplaces[i];
    // Replace value

    AddMessage('Processing '+ Name(e));
    AddMessage('Path: "' + Path + '". Match: "' + find + '". Target: "' +
                target + '". Replace: "' + replace + '".');
    // Remove trailing slash
    If Pos('\', path) = Length(path) Then path := Copy(path, 1, Length(path) - 1);

    // create lists for query path
    lst := TList.Create;
    sl := TStringList.Create;
    // Elements matched
    spl := TStringList.Create;
    // Split indexed path
    ElementsByMIP(lst, e, path);
    mgeev(sl, lst);

    // match find
    For j := 0 To Pred(sl.Count) Do Begin
      element := ObjectToElement(lst[j]);
      If Assigned(element) Then Begin
        currentIndexedPath := MyIndexedPath(element);
        If find <> '' Then Begin
          If RegexMatch(sl[j], find) Then Begin
            // End of search;
            val := sl[j];
            CheckTarget(val, target, filename, category,
                        recordEditorId, replace, e, currentIndexedPath, spl, element);
          End;
        End Else If find = '' Then Begin
          // End of search
          val := sl[j];
          CheckTarget(val, target, filename, category,
                      recordEditorId, replace, e,
                      currentIndexedPath, spl, element);
        End;
      End;
    End;
  End;
  // free memory
  lst.Free;
  sl.Free;
  spl.Free;
End;

//=========================================================================
// Finalize: Save output files, free objects.
Function finalize: integer;
Var 
  jsonOutputPath: string;
Begin
  If eJson Then Begin
    jsonOutputPath := ProgramPath + 'output.json';
    jsonObj.SaveToFile(jsonOutputPath);
    AddMessage('JSON exported to: ' + jsonOutputPath);
  End;


  slPaths.Free;
  slMatches.Free;
  slTargets.Free;
  slReplaces.Free;
  lstPath.Free;
  lstMatch.Free;
  lstTarget.Free;
  lstReplace.Free;

  jsonObj.Free;
End;

End.
