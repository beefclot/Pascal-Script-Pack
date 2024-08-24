{
  AllConditionsMatchSearch v2.0.0
  created by COACHWICKWACK
  
  *CHANGES*
  v2.0.0
    - Updated for xEdit 4.1.x
    - Fixed critical bug in logic, will now work properly
  v1.0.1
    - Renamed window to match script name
  v1.0.0
    - Initial implementation
  
  This script can be used to check for a set of AND queries for subrecord paths under a record.
  The "Match Regex" column is where you can optionally write regex to match for any subrecords found.
  The "Path Target" column lets you check after having found a path query if another target element exists.
  Optionally outputs to a JSON.
}

Unit AllConditionsMatchSearch;

Uses xEditAPI, Classes, SysUtils, ASSLib;

Const 
  // sTypes = 'string'#13'integer'#13'int64'#13'cardinal'#13'array';
  vs = 'v2';
  editvalues = true;
  // don't change this unless you know what you're doing
  nativevalues = false;
  // importing only supports edit values
  panelwidth = Screen.Width/3;
  panelheight = Screen.Height * 0.2;
  plusminuswidth = panelwidth/10;
  plusminusgap = panelwidth/30;
  generalgap = plusminusgap;
  buttonswidth = panelwidth/10;
  captionsTop = panelheight*0.1;
  rowtop = captionsTop + panelheight*0.1;
  rowleft = panelwidth/20;
  rowwidth = panelwidth - rowleft*2;
  rowheight = panelheight/25;

Var 
  i, j: integer;
  path, find, target: String;
  outputPath: String;
  eJson: Boolean;
  frm: TForm;
  pnlBottom: TPanel;
  btnOk, btnCancel, btnPlus, btnMinus: TButton;
  cb1, cb2: TCheckBox;
  lbl1, lbl2, leftlbl, middlelbl, rightlbl: TLabel;
  slPaths, slMatches, slTargets: TStringList;
  lstPath, lstMatch, lstTarget: TList;
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
  ed, em, et: TEdit;
Begin
  ed := TEdit.Create(frm);
  // Query Path
  ed.Parent := frm;
  ed.Left := rowleft;
  ed.Top := rowtop + (generalgap * lstPath.Count);
  ed.Width := rowwidth*0.4 - generalgap;

  em := TEdit.Create(frm);
  // Regex Query
  em.Parent := frm;
  em.Left := ed.Left + ed.Width + generalgap;
  em.Top := ed.Top;
  em.Width := rowwidth*0.2 - generalgap;

  et := TEdit.Create(frm);
  // Target Path
  et.Parent := frm;
  et.Left := em.Left + em.Width + generalgap;
  et.Top := em.Top;
  et.Width := rowwidth*0.4 - generalgap;

  // Add entries
  lstPath.Add(ed);
  lstMatch.Add(em);
  lstTarget.Add(et);
End;

//=========================================================================
// DelPathEntry: Deletes the bottom path entry
Procedure DelPathEntry;
Begin
  If lstPath.Count > 0 Then
    Begin
      TEdit(lstPath[Pred(lstPath.Count)]).Free;
      TEdit(lstMatch[Pred(lstMatch.Count)]).Free;
      TEdit(lstTarget[Pred(lstTarget.Count)]).Free;
      lstPath.Delete(Pred(lstPath.Count));
      lstMatch.Delete(Pred(lstMatch.Count));
      lstTarget.Delete(Pred(lstTarget.Count));
    End;
End;

//=========================================================================
// PathManager: Adds or deletes path entries
Procedure frm.PathManager(Sender: TObject);
Begin
  If (Sender = btnPlus) Then
    Begin
      AddPathEntry;
      frm.Height := frm.Height + generalgap;
    End;
  If (Sender = btnMinus) And (lstPath.Count > 0) Then
    Begin
      DelPathEntry;
      frm.Height := frm.Height - generalgap;
    End;
End;

{
  Procedure for prompting user options
}
Procedure OptionsForm;
Begin
  frm := TForm.Create(Nil);
  Try
    frm.Caption := 'AllConditionsMatchSearch ' + vs;
    frm.Width := panelwidth;
    frm.Height := panelheight;
    frm.Scaled := true;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;

    pnlBottom := TPanel.Create(frm);
    pnlBottom.Parent := frm;
    pnlBottom.BevelOuter := bvNone;
    pnlBottom.Align := alBottom;
    pnlBottom.Height := panelheight/2;

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
    lbl1.Caption := 'Enter the paths you want to find values ';
    lbl1.Top := 0;
    lbl1.Left := (panelwidth - lbl1.Width)/2;
    lbl1.Height := panelheight/5;

    lbl2 := TLabel.Create(frm);
    lbl2.Parent := pnlBottom;
    lbl2.Caption := 'Export options:';
    lbl2.Top := panelheight/4.5;
    lbl2.Left := rowleft;
    lbl2.AutoSize := False;
    lbl2.Wordwrap := True;
    lbl2.Width := 360;

    leftlbl := TLabel.Create(frm);
    leftlbl.Parent := frm;
    leftlbl.Caption := 'Path Query';
    leftlbl.Top := captionsTop;
    leftlbl.Left := rowleft;
    leftlbl.AutoSize := False;
    leftlbl.Wordwrap := True;
    leftlbl.Width := rowwidth*0.4 - generalgap;

    middlelbl := TLabel.Create(frm);
    middlelbl.Parent := frm;
    middlelbl.Caption := 'Match Regex';
    middlelbl.Top := captionsTop;
    middlelbl.Left := leftlbl.Left + leftlbl.Width + generalgap;
    middlelbl.AutoSize := False;
    middlelbl.Wordwrap := True;
    middlelbl.Width := rowwidth*0.2 - generalgap;

    rightlbl := TLabel.Create(frm);
    rightlbl.Parent := frm;
    rightlbl.Caption := 'Path Target';
    rightlbl.Top := captionsTop;
    rightlbl.AutoSize := False;
    rightlbl.Wordwrap := True;
    rightlbl.Width := (panelwidth - generalgap * 2) / 3;
    rightlbl.Left := middlelbl.Left + middlelbl.Width + generalgap;

    cb1 := TCheckBox.Create(frm);
    cb1.Parent := pnlBottom;
    cb1.Caption := ' Export as .json';
    cb1.Top := panelheight/3;
    cb1.Left := rowleft + generalgap;

    btnOk := TButton.Create(frm);
    btnOk.Parent := pnlBottom;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := (panelwidth - generalgap*2)/2 - buttonswidth;
    btnOk.Width := buttonswidth;
    btnOk.Top := panelheight/4;

    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnlBottom;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + generalgap;
    btnCancel.Width := buttonswidth;
    btnCancel.Top := panelheight/4;

    AddPathEntry;
    frm.Height := frm.Height + generalgap*(lstPath.Count + 1);
    // Spawn 1 row for entry

    If frm.ShowModal = mrOk Then
      Begin
        For i := 0 To Pred(lstPath.Count) Do
          Begin
            If SameText(TEdit(lstPath[i]).Text, '') Then Continue;

            slPaths.Add(TEdit(lstPath[i]).Text);
            slMatches.Add(TEdit(lstMatch[i]).Text);
            slTargets.Add(TEdit(lstTarget[i]).Text);
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
  slPaths := TStringList.Create;
  // Query Paths
  slMatches := TStringList.Create;
  // Regex Matches
  slTargets := TStringList.Create;
  // Target element paths
  lstPath := TList.Create;
  lstMatch := TList.Create;
  lstTarget := TList.Create;
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

Procedure ProcessTarget(filename: String; category: String; recordEditorId:
                        String; target: String; e: IInterface);
Var 
  tsl, tspl: TStringList;
  tlst: TList;
  j: integer;
  val, currentIndexedPath: string;
  element: IInterface;
Begin
  // Clean target path
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
    if Assigned(element) then begin
      val := tsl[j];
      MatchFound(val);
      
      if eJson then begin
        currentIndexedPath := MyIndexedPath(element);
        tspl := SplitStrToListFunc(currentIndexedPath, '\', tspl);
        AddElementToJson(tspl, jsonObj, filename, category, recordEditorId, val);
      end;
    end;
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
  i, j, lastIndex: integer;
  val, filename, category, recordEditorId, currentIndexedPath: string;
  element: IInterface;
  isMatched, isFound: Boolean;
Begin
  // Build json like {filename: {category: {recordEditorId: {IndexedPath}}}}
  filename := BaseName(GetFile(e));
  category := Signature(e);
  recordEditorId := EditorID(e);

  If slPaths.Count = 0 Then exit;

  isMatched := False;
  // For every element under every given query path, check that an element is assigned and if there's a regex match for any of those elements
  // For every query path
  For i := 0 To Pred(slPaths.Count) Do Begin
    // Query Path
    path := slPaths[i];
    // Regex Match
    find := slMatches[i];
    // Target path
    target := slTargets[i];

    If Pos('\', path) = Length(path) Then path := Copy(path, 1, Length(path) - 1);

    // create lists for query path
    lst := TList.Create;
    // Query Path
    sl := TStringList.Create;
    // Elements matched
    spl := TStringList.Create;
    // Split indexed path
    ElementsByMIP(lst, e, path);
    mgeev(sl, lst);

    // For every element under query path, check for regex match
    For j := 0 To Pred(sl.Count) Do Begin
      element := ObjectToElement(lst[j]);
      If not Assigned(element) Then Continue;
      
      currentIndexedPath := MyIndexedPath(element);
      if find <> '' then begin
        // If regex match exists
        isMatched := RegexMatch(sl[j], find);
        if isMatched then begin
          // If second query is filled, look for element
          if target <> '' then begin
            ProcessTarget(filename, category, recordEditorId, target, e);
          end else if target = '' then begin
            val := sl[j];
            MatchFound(val);
            
            if eJson then begin
              currentIndexedPath := MyIndexedPath(element);
              spl := SplitStrToListFunc(currentIndexedPath, '\', spl);
              AddElementToJson(spl, jsonObj, filename, category, recordEditorId, val);
            end;
            
            Break;
          end;
        end else if not isMatched then begin
          Break;
        end;
      end else if find = '' then begin
        if target <> '' then begin
          ProcessTarget(filename, category, recordEditorId, target, e);
        end else if target = '' then begin
          val := sl[j];
          MatchFound(val);
          
          if eJson then begin
            currentIndexedPath := MyIndexedPath(element);
            spl := SplitStrToListFunc(currentIndexedPath, '\', spl);
            AddElementToJson(spl, jsonObj, filename, category, recordEditorId, val);
          end;
          
          Break;
        end;
      end;
    End;

    // free memory
    lst.Free;
    sl.Free;
    spl.Free;
  End;
End;

Procedure MatchFound(myResult: String);
Begin
  AddMessage('	Match found on ' + myResult);
End;

//=========================================================================
// Finalize: Save output files, free stringlists.
// Should be used instead for processing all selected subrecords, filtering DOWN in sequential searching... eventually
Function finalize: integer;
Var 
  jsonOutputPath: string;
Begin
  If eJson Then
    Begin
      jsonOutputPath := ProgramPath + 'output.json';
      jsonObj.SaveToFile(jsonOutputPath);
      AddMessage('JSON exported to: ' + jsonOutputPath);
    End;

  slPaths.Free;
  slMatches.Free;
  slTargets.Free;
  lstPath.Free;
  lstMatch.Free;
  jsonObj.Free;
End;

End.
