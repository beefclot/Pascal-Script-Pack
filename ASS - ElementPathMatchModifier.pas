{
  ElementPathMatchModifier v2.0.0
  created by COACHWICKWACK
  
  *CHANGES*
  v2.0.0
    - Updated for xEdit v4.1.x
    - Modified handling
  v1.2.0
    - ElementDataAddRemove -> ElementPathMatchModifier
  v1.1.0
    - Moved core functions and procedures to ASSLib
    - Fixed bug with indexing indexes in structs
  v1.0.0
    - Initial implementation
  
  This script can be used to add/remove subrecord elements in records where an optional regex value can be matched.
}
unit ElementPathMatchModifier;

// Include the xEdit scripting framework
uses xEditAPI, Classes, SysUtils, ASSLib;

const
	// sTypes = 'string'#13'integer'#13'int64'#13'cardinal'#13'array';
	vs = 'v2';
	editvalues = true; // don't change this unless you know what you're doing
	nativevalues = false; // importing only supports edit values
	panelwidth = 800;
  plusminuswidth = 55;
  plusminusgap = 5;
  generalgap = 8;
  rowwidth = panelwidth - generalgap*6;
  buttonswidth = 166;

var
	i, j: integer;
	path, find, target, operation: String;
	outputPath: String;
	eJson: Boolean;
	frm: TForm;
	pnlBottom: TPanel;
	btnOk, btnCancel, btnPlus, btnMinus: TButton;
	// cb1,
  cb2: TCheckBox;
	lbl1, lbl2, leftlbl, middlelbl, rightlbl, operationlbl, limitlbl: TLabel;
	slPaths, slMatches, slTargets, slOperations, slLimits: TStringList;
	lstPath, lstMatch, lstTarget, lstOperate, lstLimit: TList;
  // jsonObj: TJsonObject;

//=========================================================================
// AddPathEntry: Creates a new path entry
procedure AddPathEntry;
var
  queryPath, em, et, operationLimit: TEdit;
  cmbContainer: TComboBox;
begin
	queryPath := TEdit.Create(frm); // Query Path
	queryPath.Parent := frm;
	queryPath.Left := 8;
	queryPath.Top := 50 + (30 * lstPath.Count);
	queryPath.Width := rowwidth*0.3;

	em := TEdit.Create(frm); // Regex Query
	em.Parent := frm;
	em.Left := queryPath.Left + queryPath.Width + 8;
	em.Top := 50 + (30 * lstPath.Count);
	em.Width := rowwidth*0.2;

  et := TEdit.Create(frm); // Target Path
  et.Parent := frm;
	et.Left := em.Left + em.Width + 8;
	et.Top := 50 + (30 * lstPath.Count);
	et.Width := rowwidth*0.3;

  cmbContainer := TComboBox.Create(frm); // Combo box for operations (add/remove)
  cmbContainer.Parent := frm;
  cmbContainer.Left := et.Left + et.Width + 8;
  cmbContainer.Top := 50 + (30 * lstPath.Count);
  cmbContainer.Width := 70;
  cmbContainer.Style := csDropDownList;
  // cmbContainer.Anchors := [akTop, akRight];
  cmbContainer.Items.Add('Add');
  cmbContainer.Items.Add('Remove');
  cmbContainer.ItemIndex := 0;

  operationLimit := TEdit.Create(frm); // Target Path
  operationLimit.Parent := frm;
	operationLimit.Left := cmbContainer.Left + cmbContainer.Width + 8;
	operationLimit.Top := 50 + (30 * lstPath.Count);
	operationLimit.Width := panelwidth - rowwidth + 24;
  operationLimit.Text := '0';

  // Add FMX objects
	lstPath.Add(queryPath);
	lstMatch.Add(em);
  lstTarget.Add(et);
  lstOperate.Add(cmbContainer);
  lstLimit.Add(operationLimit);
end;

//=========================================================================
// DelPathEntry: Deletes the bottom path entry
procedure DelPathEntry;
begin
  if lstPath.Count > 0 then begin
    TEdit(lstPath[Pred(lstPath.Count)]).Free;
    TEdit(lstMatch[Pred(lstMatch.Count)]).Free;
    TEdit(lstTarget[Pred(lstTarget.Count)]).Free;
    TComboBox(lstOperate[Pred(lstOperate.Count)]).Free;
    TEdit(lstLimit[Pred(lstLimit.Count)]).Free;

    lstPath.Delete(Pred(lstPath.Count));
	  lstMatch.Delete(Pred(lstMatch.Count));
	  lstTarget.Delete(Pred(lstTarget.Count));
	  lstOperate.Delete(Pred(lstOperate.Count));
	  lstLimit.Delete(Pred(lstLimit.Count));
  end;
end;

//=========================================================================
// PathManager: Adds or deletes path entries
procedure frm.PathManager(Sender: TObject);
begin
  if (Sender = btnPlus) then begin
    AddPathEntry;
    frm.Height := 240 + 30*(lstPath.Count);
  end;
  if (Sender = btnMinus) and (lstPath.Count > 1) then begin
    DelPathEntry;
    frm.Height := 240 + 30*(lstPath.Count);
  end;
end;

{
  Procedure for prompting user options
}
procedure OptionsForm;
begin
  frm := TForm.Create(nil);
  try
    frm.Caption := 'ElementPathMatchModifier ' + vs;
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
    lbl1.Caption := 'Enter the paths you want to modify';

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

    operationlbl := TLabel.Create(frm);
    operationlbl.Parent := frm;
    operationlbl.Top := 30;
    operationlbl.Left := panelwidth + 24 - rightlbl.Width;
    operationlbl.AutoSize := False;
    operationlbl.Wordwrap := True;
    operationlbl.Width := (panelwidth - 40) / 4;
    operationlbl.Caption := 'Operation';

    limitlbl := TLabel.Create(frm);
    limitlbl.Parent := frm;
    limitlbl.Top := 30;
    limitlbl.Left := 8 + operationlbl.Left + 70;
    limitlbl.AutoSize := False;
    limitlbl.Wordwrap := True;
    limitlbl.Width := (panelwidth - 40) / 4;
    limitlbl.Caption := 'Limit';
    
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

	  AddPathEntry;

    if frm.ShowModal = mrOk then begin
      for i := 0 to Pred(lstPath.Count) do begin
        if SameText(TEdit(lstPath[i]).Text, '') then Continue;

        // Add entries from UI to actual lists
        slPaths.Add(TEdit(lstPath[i]).Text);
		    slMatches.Add(TEdit(lstMatch[i]).Text);
        slTargets.Add(TEdit(lstTarget[i]).Text);
        slOperations.Add(TEdit(lstOperate[i]).Text);
        slLimits.Add(TEdit(lstLimit[i]).Text);
      end;
      // if cb1.Checked = cbChecked then eJson := true;
    end;
  finally
    frm.Free;
  end;
end;

function Initialize: integer;
begin
	// Welcome messages
	AddMessage('-------------------------------------------------------------------------------');

	// Object creation
	lstPath := TList.Create;
	lstMatch := TList.Create;
  lstTarget := TList.Create;
  lstOperate := TList.Create;
  lstLimit := TList.Create;

	slPaths := TStringList.Create; // Query Paths
	slMatches := TStringList.Create; // Regex Matches
  slTargets := TStringList.Create; // Target element paths
  slOperations := TStringList.Create; // Operation values
  slLimits := TStringList.Create; // Operation Limit values

  // jsonObj := TJsonObject.Create;
	AddMessage('Objects created.'+#13#10);
	
	// options form
	OptionsForm;
	// Invalid if check below... Can search for empty paths
	if slPaths.Count > 0 then 
		AddMessage(#13 + 'Processing records...')
	else 
		AddMessage('No paths specified, terminating script.');
end;

Procedure AddRemoveElement(element: IInterface; operation: string; limit: integer);
var
  currentIndexedPath, currentElementType, currentContainerType: string;
  currentContainer, operatingElement: IInterface;
  i: integer;
begin
  if Assigned(element) then begin
    currentIndexedPath := MyIndexedPath(element);
    currentElementType := ElementTypeString(element);
    currentContainer := GetContainer(element);
    currentContainerType := ElementTypeString(currentContainer);

    AddMessage('	Target "' + currentIndexedPath  + '" -> ' + operation);
    AddMessage('	element type: ' + currentElementType);
    AddMessage('	currentContainer type: ' + currentContainerType);

    if operation = 'Add' then begin// AddMessage('Inside else or block');
      operatingElement := element;

      // AddMessage('Inside Add Block with limit: ' + IntToStr(limit));
      if IsSorted(operatingElement) then begin
        // AddMessage('Inside IsSorted Add Block with limit: ' + IntToStr(limit));
        for i := 0 to Pred(limit) do begin
          ElementAssign(operatingElement, HighInteger, Nil, False);
        end;
        exit;
      end else begin
        for i := 0 to Pred(limit) do begin
          ElementAssign(operatingElement, HighInteger, Nil, False);
        end;
        exit;
      end;

    end else if operation = 'Remove' then begin
      // AddMessage('Inside Remove Block');
      if currentElementType = 'etSubRecord' then begin // currentContainer = Container of element
        operatingElement := element;
        RemoveElement(currentContainer, element);
      end else begin // element = etSubRecordArray or struct or some iterable
        for i := 0 to Pred(limit) do begin
          operatingElement := ElementByIndex(element, 0);
          RemoveElement(element, operatingElement);
        end;
      end;
      exit;
    end;
  end;
end;

Procedure CheckTarget(val: string; target: string; filename: string; category: string; 
recordEditorId: string; operation: string; e: IInterface; currentIndexedPath: string; 
spl: TStringList; element: IInterface; limit: integer);
begin
  if target <> '' then begin
    ProcessTarget(filename, category, recordEditorId, target, operation, e, limit);
  end
  else if target = '' then begin
    AddRemoveElement(element, operation, limit);
  end;
end;

Procedure ProcessTarget(filename: string; category: string; recordEditorId: string; 
target: string; operation: string; e: IInterface; limit: integer);
var
  tspl: TStringList;
  tlst: TList;
	j: integer;
	val, currentIndexedPath, currentElementType, currentContainerType: string;
	element, currentContainer, operatingElement: IInterface;
begin
  // AddMessage('Processing target for '+ Name(e));
  // Remove trailing slash
  if Pos('\', target) = Length(target) then target := Copy(target, 1, Length(target) - 1);

  // create lists for target path
  tspl := TStringList.Create; // Split indexed path
  target := PrepareIP(target, tspl); // Cleaned target path
  tlst := TList.Create; // Elements list
  
  ElementsByMIPWithoutPrepareIP(tlst, e, tspl);

  // match find
  for j := 0 to Pred(tlst.Count) do begin
    element := ObjectToElement(tlst[j]);
    AddRemoveElement(element, operation, limit);
  end;
  
  // free memory
  tlst.Free;
  tspl.Free;
end;

function Process(e: IInterface): integer;
var
	lst, tlst: TList;
	sl, spl, tsl: TStringList;
	i, j, limit: integer;
	val, filename, category, recordEditorId, currentIndexedPath: string;
	element: IInterface;
begin
  // Build json like {filename: {category: {recordEditorId: {IndexedPath}}}}
  filename := BaseName(GetFile(e));
  category := Signature(e);
  recordEditorId := EditorID(e);

	if slPaths.Count = 0 then exit;
  for i := 0 to Pred(slPaths.Count) do begin
    path := slPaths[i]; // Query Path
    find := slMatches[i]; // Regex Match
    target := slTargets[i]; // Target path
    operation := slOperations[i]; // Add or Remove, as a string
    limit := StrToInt(slLimits[i]); // Limit of operation

    AddMessage('Processing '+ Name(e));
    if Pos('\', path) = Length(path) then path := Copy(path, 1, Length(path) - 1);

    // create lists for query path
    lst := TList.Create; // Elements output list
    sl := TStringList.Create; // Element values
    spl := TStringList.Create; // Split indexed path
    ElementsByMIP(lst, e, path);

    mgeev(sl, lst);

    for j := 0 to Pred(lst.Count) do begin // For each element found with path
      element := ObjectToElement(lst[j]);

      if Assigned(element) then begin
        currentIndexedPath := MyIndexedPath(element);
        if find <> '' then begin
          if RegexMatch(sl[j], find) then begin // End of search;
            CheckTarget(val, target, filename, category, recordEditorId, 
            operation, e, currentIndexedPath, spl, element, limit);
          end;
        end
        else if find = '' then begin // End of search
          CheckTarget(val, target, filename, category, recordEditorId, 
          operation, e, currentIndexedPath, spl, element, limit);
        end; 
      end;
    end;

    // free memory
    lst.Free;
    sl.Free;
    spl.Free;
	end;
end;

//=========================================================================
// Finalize: Save output files, free objects.
function finalize: integer;
var
  jsonOutputPath: string;
begin
  // if eJson then begin
  //   jsonOutputPath := ProgramPath + 'output.json';
  //   jsonObj.SaveToFile(jsonOutputPath);
  //   AddMessage('JSON exported to: ' + jsonOutputPath);
  // end;

  slPaths.Free;
	slMatches.Free;
  slTargets.Free;
  slOperations.Free;
  slLimits.Free;

	lstPath.Free;
	lstMatch.Free;
  lstTarget.Free;
  lstOperate.Free;
  lstLimit.Free;

  // jsonObj.Free;
end;

end.
