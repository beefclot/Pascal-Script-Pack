{
  AllConditionsMatchFilter v2.0.0
  created by COACHWICKWACK
  
  *CHANGES*
  v2.0.0
    - Updated for xEdit 4.1.x
    - Fixed critical logic bug for checking ALL queries end up with a truthy result
  v1.0.0
    - Initial implementation
  
  This script can be used to do a sequential vector filter for matched cells in several subrecords, based on a record's element path(s) and optional regex matches.
}

Unit AllConditionsMatchFilter;

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

Var 
  i, j: integer;
  path, find, target: String;
  frm: TForm;
  pnlBottom: TPanel;
  scrollBoxOptions: TScrollBox;
  btnOk, btnCancel, btnPlus, btnMinus: TButton;
  lbl1, leftlbl, middlelbl, rightlbl: TLabel;
  slPaths, slMatches, slTargets: TStringList;
  lstPath, lstMatch, lstTarget: TList;

  FilterEditorIDString, FilterNameString, FilterBaseEditorIDString, FilterBaseNameString, FilterSignaturesString: TEdit;
  FilterConflictAllCB, FilterConflictThisCB, FilterByInjectStatusCB, FilterInjectStatusCB, FilterByNotReachableStatusCB: TCheckBox;
  FilterNotReachableStatusCB, FilterByReferencesInjectedStatusCB, FilterReferencesInjectedStatusCB: TCheckBox;
  FilterByEditorIDCB, FilterByNameCB, FilterByBaseEditorIDCB, FilterByBaseNameCB: TCheckBox;
  FilterScaledActorsCB, FilterByPersistentCB, FilterPersistentCB, FilterUnnecessaryPersistentCB: TCheckBox;
  FilterMasterIsTemporaryCB, FilterIsMasterCB, FilterPersistentPosChangedCB, FilterDeletedCB: TCheckBox;
  FilterByVWDCB, FilterVWDCB, FilterByHasVWDMeshCB, FilterHasVWDMeshCB, FilterBySignatureCB: TCheckBox;
  FilterByBaseSignatureCB, FlattenBlocksCB, FlattenCellChildsCB, AssignPersWrldChildCB, InheritConflictByParentCB: TCheckBox;

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
  ed.Width := rowwidth*1/3 - generalgap;

  em := TEdit.Create(frm);
  // Regex Query
  em.Parent := frm;
  em.Left := ed.Left + ed.Width + generalgap;
  em.Top := ed.Top;
  em.Width := rowwidth*1/3 - generalgap;

  et := TEdit.Create(frm);
  // Target Path
  et.Parent := frm;
  et.Left := em.Left + em.Width + generalgap;
  et.Top := em.Top;
  et.Width := rowwidth*1/3 - generalgap;

  // Add entries
  lstPath.Add(ed);
  lstMatch.Add(em);
  lstTarget.Add(et);
End;

//=========================================================================
// DelPathEntry: Deletes the bottom path entry
Procedure DelPathEntry;
Begin
  If lstPath.Count > 0 Then Begin
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
  If (Sender = btnPlus) Then Begin
    AddPathEntry;
    frm.Height := frm.Height + generalgap;
  End;
  If (Sender = btnMinus) And (lstPath.Count > 0) Then Begin
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
    frm.Caption := 'AllConditionsMatchFilter ' + vs;
    frm.Width := panelwidth;
    frm.Height := panelheight;
    frm.Scaled := true;
    frm.Position := poScreenCenter;
    frm.BorderStyle := bsDialog;

    pnlBottom := TPanel.Create(frm);
    pnlBottom.Parent := frm;
    // pnlBottom.BevelOuter := bvNone;
    pnlBottom.Align := alBottom;
    pnlBottom.Height := panelheight/2;

    btnPlus := TButton.Create(frm);
    btnPlus.Parent := pnlBottom;
    btnPlus.Caption := '+';
    btnPlus.Width := (plusminuswidth - plusminusgap)/2;
    btnPlus.Left := (panelwidth - plusminuswidth)*3/4;
    btnPlus.Top := 35;
    btnPlus.OnClick := PathManager;

    btnMinus := TButton.Create(frm);
    btnMinus.Parent := pnlBottom;
    btnMinus.Caption := '-';
    btnMinus.Width := (plusminuswidth - plusminusgap)/2;
    btnMinus.Left := btnPlus.Left + btnMinus.Width + generalgap/2;
    btnMinus.Top := 35;
    btnMinus.OnClick := PathManager;

    lbl1 := TLabel.Create(frm);
    lbl1.Parent := frm;
    lbl1.Caption := 'Enter the paths you want to filter for matched values ';
    lbl1.Top := 0;
    lbl1.Left := (panelwidth - lbl1.Width)/2;
    lbl1.Height := panelheight/5;

    leftlbl := TLabel.Create(frm);
    leftlbl.Parent := frm;
    leftlbl.Caption := 'Path Query';
    leftlbl.Top := captionsTop;
    leftlbl.Left := rowleft;
    leftlbl.AutoSize := False;
    leftlbl.Width := rowwidth/3 - generalgap;

    middlelbl := TLabel.Create(frm);
    middlelbl.Parent := frm;
    middlelbl.Caption := 'Match Regex';
    middlelbl.Top := captionsTop;
    middlelbl.Left := leftlbl.Left + leftlbl.Width + generalgap;
    middlelbl.AutoSize := False;
    middlelbl.Width := rowwidth/3 - generalgap;

    rightlbl := TLabel.Create(frm);
    rightlbl.Parent := frm;
    rightlbl.Caption := 'Path Target';
    rightlbl.Top := captionsTop;
    rightlbl.AutoSize := False;
    rightlbl.Width := rowwidth/3 - generalgap;
    rightlbl.Left := middlelbl.Left + middlelbl.Width + generalgap;

    btnOk := TButton.Create(frm);
    btnOk.Parent := pnlBottom;
    btnOk.Caption := 'OK';
    btnOk.ModalResult := mrOk;
    btnOk.Left := (panelwidth - generalgap*2)*3/4 - buttonswidth;
    btnOk.Width := buttonswidth;
    btnOk.Top := panelheight/4;

    btnCancel := TButton.Create(frm);
    btnCancel.Parent := pnlBottom;
    btnCancel.Caption := 'Cancel';
    btnCancel.ModalResult := mrCancel;
    btnCancel.Left := btnOk.Left + btnOk.Width + generalgap;
    btnCancel.Width := buttonswidth;
    btnCancel.Top := panelheight/4;

    scrollBoxOptions := TScrollBox.Create(frm);
    scrollBoxOptions.Parent := pnlBottom;
    scrollBoxOptions.Height := pnlBottom.Height;
    scrollBoxOptions.Width := panelwidth/2;

    FilterConflictAllCB := TCheckBox.Create(frm);
    FilterConflictAllCB.Parent := scrollBoxOptions;
    FilterConflictAllCB.Caption := 'Filter Conflict All';
    FilterConflictAllCB.Checked := False;

    FilterConflictThisCB := TCheckBox.Create(frm);
    FilterConflictThisCB.Parent := scrollBoxOptions; 
    FilterConflictThisCB.Caption := 'Filter Conflict This';
    FilterConflictThisCB.Checked := False;
    FilterConflictThisCB.Top := FilterConflictAllCB.Top + FilterConflictAllCB.Height;
    FilterConflictThisCB.Width := frm.Canvas.TextWidth(FilterConflictThisCB.Caption) + 20;

    FilterByInjectStatusCB := TCheckBox.Create(frm);
    FilterByInjectStatusCB.Parent := scrollBoxOptions;
    FilterByInjectStatusCB.Caption := 'Filter By Inject Status';
    FilterByInjectStatusCB.Checked := False;
    FilterByInjectStatusCB.Top := FilterConflictThisCB.Top + FilterConflictThisCB.Height;
    FilterByInjectStatusCB.Width := frm.Canvas.TextWidth(FilterByInjectStatusCB.Caption) + 20;

    FilterInjectStatusCB := TCheckBox.Create(frm);
    FilterInjectStatusCB.Parent := scrollBoxOptions;
    FilterInjectStatusCB.Caption := 'Filter Inject Status';
    FilterInjectStatusCB.Checked := False;
    FilterInjectStatusCB.Top := FilterByInjectStatusCB.Top + FilterByInjectStatusCB.Height;
    FilterInjectStatusCB.Width := frm.Canvas.TextWidth(FilterInjectStatusCB.Caption) + 20;

    FilterByNotReachableStatusCB := TCheckBox.Create(frm);
    FilterByNotReachableStatusCB.Parent := scrollBoxOptions;
    FilterByNotReachableStatusCB.Caption := 'Filter By Not Reachable Status';
    FilterByNotReachableStatusCB.Checked := False;
    FilterByNotReachableStatusCB.Top := FilterInjectStatusCB.Top + FilterInjectStatusCB.Height;
    FilterByNotReachableStatusCB.Width := frm.Canvas.TextWidth(FilterByNotReachableStatusCB.Caption) + 20;

    FilterNotReachableStatusCB := TCheckBox.Create(frm);
    FilterNotReachableStatusCB.Parent := scrollBoxOptions;
    FilterNotReachableStatusCB.Caption := 'Filter Not Reachable Status';
    FilterNotReachableStatusCB.Checked := False;
    FilterNotReachableStatusCB.Top := FilterByNotReachableStatusCB.Top + FilterByNotReachableStatusCB.Height;
    FilterNotReachableStatusCB.Width := frm.Canvas.TextWidth(FilterNotReachableStatusCB.Caption) + 20;

    FilterByReferencesInjectedStatusCB := TCheckBox.Create(frm);
    FilterByReferencesInjectedStatusCB.Parent := scrollBoxOptions;
    FilterByReferencesInjectedStatusCB.Caption := 'Filter By References Injected Status';
    FilterByReferencesInjectedStatusCB.Checked := False;
    FilterByReferencesInjectedStatusCB.Top := FilterNotReachableStatusCB.Top + FilterNotReachableStatusCB.Height;
    FilterByReferencesInjectedStatusCB.Width := frm.Canvas.TextWidth(FilterByReferencesInjectedStatusCB.Caption) + 20;

    FilterReferencesInjectedStatusCB := TCheckBox.Create(frm);
    FilterReferencesInjectedStatusCB.Parent := scrollBoxOptions;
    FilterReferencesInjectedStatusCB.Caption := 'Filter References Injected Status';
    FilterReferencesInjectedStatusCB.Checked := False;
    FilterReferencesInjectedStatusCB.Top := FilterByReferencesInjectedStatusCB.Top + FilterByReferencesInjectedStatusCB.Height;
    FilterReferencesInjectedStatusCB.Width := frm.Canvas.TextWidth(FilterReferencesInjectedStatusCB.Caption) + 20;

    FilterByEditorIDCB := TCheckBox.Create(frm);
    FilterByEditorIDCB.Parent := scrollBoxOptions;
    FilterByEditorIDCB.Caption := 'Filter By Editor ID';
    FilterByEditorIDCB.Checked := False;
    FilterByEditorIDCB.Top := FilterReferencesInjectedStatusCB.Top + FilterReferencesInjectedStatusCB.Height;
    FilterByEditorIDCB.Width := frm.Canvas.TextWidth(FilterByEditorIDCB.Caption) + 20;

    FilterEditorIDString := TEdit.Create(frm);
    FilterEditorIDString.Parent := scrollBoxOptions;
    FilterEditorIDString.Top := FilterByEditorIDCB.Top;
    FilterEditorIDString.Width := 150;
    FilterEditorIDString.Left := scrollBoxOptions.Width - FilterEditorIDString.Width - 20;

    FilterByNameCB := TCheckBox.Create(frm);
    FilterByNameCB.Parent := scrollBoxOptions;
    FilterByNameCB.Caption := 'Filter By Name';
    FilterByNameCB.Checked := False;
    FilterByNameCB.Top := FilterByEditorIDCB.Top + FilterByEditorIDCB.Height;
    FilterByNameCB.Width := frm.Canvas.TextWidth(FilterByNameCB.Caption) + 20;

    FilterNameString := TEdit.Create(frm);
    FilterNameString.Parent := scrollBoxOptions;
    FilterNameString.Width := 150;
    FilterNameString.Top := FilterByNameCB.Top;
    FilterNameString.Left := scrollBoxOptions.Width - FilterNameString.Width - 20;

    FilterByBaseEditorIDCB := TCheckBox.Create(frm);
    FilterByBaseEditorIDCB.Parent := scrollBoxOptions;
    FilterByBaseEditorIDCB.Caption := 'Filter By Base Editor ID';
    FilterByBaseEditorIDCB.Checked := False;
    FilterByBaseEditorIDCB.Top := FilterByNameCB.Top + FilterByNameCB.Height;
    FilterByBaseEditorIDCB.Width := frm.Canvas.TextWidth(FilterByBaseEditorIDCB.Caption) + 20;

    FilterBaseEditorIDString := TEdit.Create(frm);
    FilterBaseEditorIDString.Parent := scrollBoxOptions;
    FilterBaseEditorIDString.Width := 150;
    FilterBaseEditorIDString.Top := FilterByBaseEditorIDCB.Top;
    FilterBaseEditorIDString.Left := scrollBoxOptions.Width - FilterBaseEditorIDString.Width - 20;

    FilterByBaseNameCB := TCheckBox.Create(frm);
    FilterByBaseNameCB.Parent := scrollBoxOptions;
    FilterByBaseNameCB.Caption := 'Filter By Base Name';
    FilterByBaseNameCB.Checked := False;
    FilterByBaseNameCB.Top := FilterByBaseEditorIDCB.Top + FilterByBaseEditorIDCB.Height;
    FilterByBaseNameCB.Width := frm.Canvas.TextWidth(FilterByBaseNameCB.Caption) + 20;

    FilterBaseNameString := TEdit.Create(frm);
    FilterBaseNameString.Parent := scrollBoxOptions;
    FilterBaseNameString.Width := 150;
    FilterBaseNameString.Top := FilterByBaseNameCB.Top;
    FilterBaseNameString.Left := scrollBoxOptions.Width - FilterBaseNameString.Width - 20;

    FilterScaledActorsCB := TCheckBox.Create(frm);
    FilterScaledActorsCB.Parent := scrollBoxOptions;
    FilterScaledActorsCB.Caption := 'Filter Scaled Actors';
    FilterScaledActorsCB.Checked := False;
    FilterScaledActorsCB.Top := FilterByBaseNameCB.Top + FilterByBaseNameCB.Height;
    FilterScaledActorsCB.Width := frm.Canvas.TextWidth(FilterScaledActorsCB.Caption) + 20;

    FilterByPersistentCB := TCheckBox.Create(frm);
    FilterByPersistentCB.Parent := scrollBoxOptions;
    FilterByPersistentCB.Caption := 'Filter By Persistent';
    FilterByPersistentCB.Checked := False;
    FilterByPersistentCB.Top := FilterScaledActorsCB.Top + FilterScaledActorsCB.Height;
    FilterByPersistentCB.Width := frm.Canvas.TextWidth(FilterByPersistentCB.Caption) + 20;
    
    FilterPersistentCB := TCheckBox.Create(frm);
    FilterPersistentCB.Parent := scrollBoxOptions;
    FilterPersistentCB.Caption := 'Filter Persistent';
    FilterPersistentCB.Checked := False;
    FilterPersistentCB.Top := FilterByPersistentCB.Top + FilterByPersistentCB.Height;
    FilterPersistentCB.Width := frm.Canvas.TextWidth(FilterPersistentCB.Caption) + 20;

    FilterUnnecessaryPersistentCB := TCheckBox.Create(frm);
    FilterUnnecessaryPersistentCB.Parent := scrollBoxOptions;
    FilterUnnecessaryPersistentCB.Caption := 'Filter Unnecessary Persistent';
    FilterUnnecessaryPersistentCB.Checked := False;
    FilterUnnecessaryPersistentCB.Top := FilterPersistentCB.Top + FilterPersistentCB.Height;
    FilterUnnecessaryPersistentCB.Width := frm.Canvas.TextWidth(FilterUnnecessaryPersistentCB.Caption) + 20;

    FilterMasterIsTemporaryCB := TCheckBox.Create(frm);
    FilterMasterIsTemporaryCB.Parent := scrollBoxOptions;
    FilterMasterIsTemporaryCB.Caption := 'Filter Master Is Temporary';
    FilterMasterIsTemporaryCB.Checked := False;
    FilterMasterIsTemporaryCB.Top := FilterUnnecessaryPersistentCB.Top + FilterUnnecessaryPersistentCB.Height;
    FilterMasterIsTemporaryCB.Width := frm.Canvas.TextWidth(FilterMasterIsTemporaryCB.Caption) + 20;

    FilterIsMasterCB := TCheckBox.Create(frm);
    FilterIsMasterCB.Parent := scrollBoxOptions;
    FilterIsMasterCB.Caption := 'Filter Is Master';
    FilterIsMasterCB.Checked := False;
    FilterIsMasterCB.Top := FilterMasterIsTemporaryCB.Top + FilterMasterIsTemporaryCB.Height;
    FilterIsMasterCB.Width := frm.Canvas.TextWidth(FilterIsMasterCB.Caption) + 20;

    FilterPersistentPosChangedCB := TCheckBox.Create(frm);
    FilterPersistentPosChangedCB.Parent := scrollBoxOptions;
    FilterPersistentPosChangedCB.Caption := 'Filter Persistent Pos Changed';
    FilterPersistentPosChangedCB.Checked := False;
    FilterPersistentPosChangedCB.Top := FilterIsMasterCB.Top + FilterIsMasterCB.Height;
    FilterPersistentPosChangedCB.Width := frm.Canvas.TextWidth(FilterPersistentPosChangedCB.Caption) + 20;

    FilterDeletedCB := TCheckBox.Create(frm);
    FilterDeletedCB.Parent := scrollBoxOptions;
    FilterDeletedCB.Caption := 'Filter Deleted';
    FilterDeletedCB.Checked := False;
    FilterDeletedCB.Top := FilterPersistentPosChangedCB.Top + FilterPersistentPosChangedCB.Height;
    FilterDeletedCB.Width := frm.Canvas.TextWidth(FilterDeletedCB.Caption) + 20;

    FilterByVWDCB := TCheckBox.Create(frm);
    FilterByVWDCB.Parent := scrollBoxOptions;
    FilterByVWDCB.Caption := 'Filter By VWD';
    FilterByVWDCB.Checked := False;
    FilterByVWDCB.Top := FilterDeletedCB.Top + FilterDeletedCB.Height;
    FilterByVWDCB.Width := frm.Canvas.TextWidth(FilterByVWDCB.Caption) + 20;

    FilterVWDCB := TCheckBox.Create(frm);
    FilterVWDCB.Parent := scrollBoxOptions;
    FilterVWDCB.Caption := 'Filter VWD';
    FilterVWDCB.Checked := False;
    FilterVWDCB.Top := FilterByVWDCB.Top + FilterByVWDCB.Height;
    FilterVWDCB.Width := frm.Canvas.TextWidth(FilterVWDCB.Caption) + 20;

    FilterByHasVWDMeshCB := TCheckBox.Create(frm);
    FilterByHasVWDMeshCB.Parent := scrollBoxOptions;
    FilterByHasVWDMeshCB.Caption := 'Filter By Has VWD Mesh';
    FilterByHasVWDMeshCB.Checked := False;
    FilterByHasVWDMeshCB.Top := FilterVWDCB.Top + FilterVWDCB.Height;
    FilterByHasVWDMeshCB.Width := frm.Canvas.TextWidth(FilterByHasVWDMeshCB.Caption) + 20;

    FilterHasVWDMeshCB := TCheckBox.Create(frm);
    FilterHasVWDMeshCB.Parent := scrollBoxOptions;
    FilterHasVWDMeshCB.Caption := 'Filter Has VWD Mesh';
    FilterHasVWDMeshCB.Checked := False;
    FilterHasVWDMeshCB.Top := FilterByHasVWDMeshCB.Top + FilterByHasVWDMeshCB.Height;
    FilterHasVWDMeshCB.Width := frm.Canvas.TextWidth(FilterHasVWDMeshCB.Caption) + 20;

    FilterBySignatureCB := TCheckBox.Create(frm);
    FilterBySignatureCB.Parent := scrollBoxOptions;
    FilterBySignatureCB.Caption := 'Filter By Signature';
    FilterBySignatureCB.Checked := False;
    FilterBySignatureCB.Top := FilterHasVWDMeshCB.Top + FilterHasVWDMeshCB.Height;
    FilterBySignatureCB.Width := frm.Canvas.TextWidth(FilterBySignatureCB.Caption) + 20;

    FilterSignaturesString := TEdit.Create(frm);
    FilterSignaturesString.Parent := scrollBoxOptions;
    FilterSignaturesString.Width := 150;
    FilterSignaturesString.Top := FilterBySignatureCB.Top;
    FilterSignaturesString.Left := scrollBoxOptions.Width - FilterSignaturesString.Width - 20;

    FilterByBaseSignatureCB := TCheckBox.Create(frm);
    FilterByBaseSignatureCB.Parent := scrollBoxOptions;
    FilterByBaseSignatureCB.Caption := 'Filter By Base Signature';
    FilterByBaseSignatureCB.Checked := False;
    FilterByBaseSignatureCB.Top := FilterBySignatureCB.Top + FilterBySignatureCB.Height;
    FilterByBaseSignatureCB.Width := frm.Canvas.TextWidth(FilterByBaseSignatureCB.Caption) + 20;

    FlattenBlocksCB := TCheckBox.Create(frm);
    FlattenBlocksCB.Parent := scrollBoxOptions;
    FlattenBlocksCB.Caption := 'Flatten Blocks';
    FlattenBlocksCB.Checked := False;
    FlattenBlocksCB.Top := FilterByBaseSignatureCB.Top + FilterByBaseSignatureCB.Height;
    FlattenBlocksCB.Width := frm.Canvas.TextWidth(FlattenBlocksCB.Caption) + 20;

    FlattenCellChildsCB := TCheckBox.Create(frm);
    FlattenCellChildsCB.Parent := scrollBoxOptions;
    FlattenCellChildsCB.Caption := 'Flatten Cell Childs';
    FlattenCellChildsCB.Checked := False;
    FlattenCellChildsCB.Top := FlattenBlocksCB.Top + FlattenBlocksCB.Height;
    FlattenCellChildsCB.Width := frm.Canvas.TextWidth(FlattenCellChildsCB.Caption) + 20;

    AssignPersWrldChildCB := TCheckBox.Create(frm);
    AssignPersWrldChildCB.Parent := scrollBoxOptions;
    AssignPersWrldChildCB.Caption := 'Assign Pers Wrld Child';
    AssignPersWrldChildCB.Checked := False;
    AssignPersWrldChildCB.Top := FlattenCellChildsCB.Top + FlattenCellChildsCB.Height;
    AssignPersWrldChildCB.Width := frm.Canvas.TextWidth(AssignPersWrldChildCB.Caption) + 20;

    InheritConflictByParentCB := TCheckBox.Create(frm);
    InheritConflictByParentCB.Parent := scrollBoxOptions;
    InheritConflictByParentCB.Caption := 'Inherit Conflict By Parent';
    InheritConflictByParentCB.Checked := True;
    InheritConflictByParentCB.Top := AssignPersWrldChildCB.Top + AssignPersWrldChildCB.Height;
    InheritConflictByParentCB.Width := frm.Canvas.TextWidth(InheritConflictByParentCB.Caption) + 20;

    // Spawn 1 row for entry
    AddPathEntry;
    frm.Height := frm.Height + generalgap*(lstPath.Count + 1);

    // If user clicks OK
    If frm.ShowModal = mrOk Then Begin
      For i := 0 To Pred(lstPath.Count) Do Begin
        If SameText(TEdit(lstPath[i]).Text, '') Then Continue;

        slPaths.Add(TEdit(lstPath[i]).Text);
        slMatches.Add(TEdit(lstMatch[i]).Text);
        slTargets.Add(TEdit(lstTarget[i]).Text);
      End;
    End;
    
  Finally
    frm.Free;
  End;
End;

function Filter(e: IInterface): Boolean;
Var 
  lst, tlst: TList;
  sl, spl, tsl, tspl: TStringList;
  i, j, lastIndex: integer;
  val, filename, category, recordEditorId, currentIndexedPath: string;
  element: IInterface;
  isMatched, targetFound: Boolean;
Begin
  // Build json like {filename: {category: {recordEditorId: {IndexedPath}}}}
  filename := BaseName(GetFile(e));
  category := Signature(e);
  recordEditorId := EditorID(e);

  If slPaths.Count = 0 Then exit;
  
  isMatched := False;

  AddMessage('Processing record: ' + filename + ' ' + category + ' ' + recordEditorId);

  // For every element under every given query path, check that an element is assigned and if there's a regex match for any of those elements
  // For every query path
  For i := 0 To Pred(slPaths.Count) Do Begin
	// Query Path
	path := slPaths[i];
	If Pos('\', path) = Length(path) Then path := Copy(path, 1, Length(path) - 1);
	// Regex Match
	find := slMatches[i];
	// Target path
	target := slTargets[i];
	
	// Json Object to track all targets found
	targetFound := False;

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
	  // If element in query path exists, try to match
	  If Assigned(element) Then Begin
      currentIndexedPath := MyIndexedPath(element);
      If find <> '' Then Begin
        // If regex match, pass the vibe check and continue
        If RegexMatch(sl[j], find) then Begin
          targetFound := True;
          Break;
        End;
      End;
	  End;
	End;
	
	If targetFound = False Then Begin
	  lst.Free;
	  sl.Free;
	  spl.Free;
	  
	  Result := False;
	  Exit;
	End;

	  // free memory
	lst.Free;
	sl.Free;
	spl.Free;
  End;

  // Optional Target find second
  lastIndex := Pred(slPaths.Count);
  path := slPaths[lastIndex];
  find := slMatches[lastIndex];
  target := slTargets[lastIndex];

  If Pos('\', path) = Length(path) Then path := Copy(path, 1, Length(path) - 1);

  lst := TList.Create;
  sl := TStringList.Create;
  spl := TStringList.Create;
  ElementsByMIP(lst, e, path);
  mgeev(sl, lst);

  For j := 0 To Pred(sl.Count) Do Begin
    element := ObjectToElement(lst[j]);
    If Assigned(element) Then Begin
      currentIndexedPath := MyIndexedPath(element);
      If find <> '' Then Begin
        If RegexMatch(sl[j], find) Then Begin
          // End of search;
          val := sl[j];
          If target <> '' Then Begin
            Result := ProcessFinalTarget(filename, category, recordEditorId, target, e)
          End Else If target = '' Then Begin
            Result := True;
          End;
        End;
      End Else If find = '' Then Begin
        // End of search
        val := sl[j];
        If target <> '' Then Begin
          Result := ProcessFinalTarget(filename, category, recordEditorId, target, e)
        End Else If target = '' Then Begin
          Result := True;
        End;
      End;
    End;
  End;
  
  // free memory
  lst.Free;
  sl.Free;
  spl.Free;
End;

Function Initialize: integer;
Begin
  // Welcome messages
  AddMessage('-------------------------------------------------------------------------------');
  // Object creation
  slPaths := TStringList.Create;
  slMatches := TStringList.Create;
  slTargets := TStringList.Create;
  lstPath := TList.Create;
  lstMatch := TList.Create;
  lstTarget := TList.Create;
  AddMessage('Objects created.'+#13#10);

  // options form
  OptionsForm;
  // Invalid if check below... Can search for empty paths
  If slPaths.Count > 0 Then Begin
    AddMessage(#13 + 'Processing records...')
  End Else Begin
    Exit;
  End;

  Result := 1;

  FilterConflictAll := FilterConflictAllCB.Checked;
  FilterConflictThis := FilterConflictThisCB.Checked;
  FilterByInjectStatus := FilterByInjectStatusCB.Checked;
  FilterInjectStatus := FilterInjectStatusCB.Checked;
  FilterByNotReachableStatus := FilterByNotReachableStatusCB.Checked;
  FilterNotReachableStatus := FilterNotReachableStatusCB.Checked;
  FilterByReferencesInjectedStatus := FilterByReferencesInjectedStatusCB.Checked;
  FilterReferencesInjectedStatus := FilterReferencesInjectedStatusCB.Checked;
  FilterByEditorID := FilterByEditorIDCB.Checked;
  FilterEditorID := FilterEditorIDString.Text;
  FilterByName := FilterByNameCB.Checked;
  FilterName := FilterNameString.Text;
  FilterByBaseEditorID := FilterByBaseEditorIDCB.Checked;
  FilterBaseEditorID := FilterBaseEditorIDString.Text;
  FilterByBaseName := FilterByBaseNameCB.Checked;
  FilterBaseName := FilterBaseNameString.Text;
  FilterScaledActors := FilterScaledActorsCB.Checked;
  FilterByPersistent := FilterByPersistentCB.Checked;
  FilterPersistent := FilterPersistentCB.Checked;
  FilterUnnecessaryPersistent := FilterUnnecessaryPersistentCB.Checked;
  FilterMasterIsTemporary := FilterMasterIsTemporaryCB.Checked;
  FilterIsMaster := FilterIsMasterCB.Checked;
  FilterPersistentPosChanged := FilterPersistentPosChangedCB.Checked;
  FilterDeleted := FilterDeletedCB.Checked;
  FilterByVWD := FilterByVWDCB.Checked;
  FilterVWD := FilterVWDCB.Checked;
  FilterByHasVWDMesh := FilterByHasVWDMeshCB.Checked;
  FilterHasVWDMesh := FilterHasVWDMeshCB.Checked;
  FilterBySignature := FilterBySignatureCB.Checked;
  FilterSignatures := FilterSignaturesString.Text;
  FilterByBaseSignature := FilterByBaseSignatureCB.Checked;
  FlattenBlocks := FlattenBlocksCB.Checked;
  FlattenCellChilds := FlattenCellChildsCB.Checked;
  AssignPersWrldChild := AssignPersWrldChildCB.Checked;
  InheritConflictByParent := InheritConflictByParentCB.Checked;
  FilterScripted := True; // use custom Filter() function

  ApplyFilter;

  slPaths.Free;
  slMatches.Free;
  slTargets.Free;
  lstPath.Free;
  lstMatch.Free;
  lstTarget.Free;

End;

Function ProcessFinalTarget(filename: String; category: String; recordEditorId:
                             String; target: String; e: IInterface): Boolean;
Var 
  tsl, tspl: TStringList;
  tlst: TList;
  j, lastIndex: integer;
  val, currentIndexedPath: string;
  element: IInterface;
Begin
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

  lastIndex := Pred(tsl.Count);
  // Use lastIndex as needed
  element := ObjectToElement(tlst[lastIndex]);
  If Assigned(element) Then Begin
    Result := True;
  End Else Begin
    tlst.Free;
    tsl.Free;
    tspl.Free;
    Result := False;
  End;

  // free memory
  tlst.Free;
  tsl.Free;
  tspl.Free;
End;

End.
