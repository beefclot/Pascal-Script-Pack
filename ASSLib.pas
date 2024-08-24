{
  Library used for Awesome Sauce Scripting (ASS) Power Pack, borrows a lot from mteFunctions and includes updated ElementsByMIP to fix index bug with structs
  
  *CHANGES*
  v2.0.0
    - Updated scripts for xEdit 4.1.x
    - IndexedPath -> MyIndexedPath
  v1.1.1
    - Fixed major bug with etSubRecordStruct or etArray elements not indexing properly and with not writing JSON in arrays accordingly

  v1.1.0
    - Moved core functions and procedures to ASSLib
    - Fixed bug with indexing elements in structs
    - Initial implementation

  Methods used from mteFunctions (* are modified):
    MyIndexedPath
    ElementTypeString
    DelimitedTextBetween
    GetTextIn
    GetChar
    CopyFromTo
    ElementsByMIP*
    mgeev
    RemoveFromEnd
    RemoveFromStart
}
unit ASSLib;

{
  IndexedPath:
  Gets the indexed path of an element.
  
  Example usage:
  element := ElementByPath(e, 'Conditions\[3]\CTDA - \Comparison Value');
  AddMessage(IndexedPath(element)); //Conditions\[3]\CTDA - \Comparison Value
}
function MyIndexedPath(e: IInterface): string;
var
  c: IInterface;
  a: string;
begin
  c := GetContainer(e);
  while (ElementTypeString(e) <> 'etMainRecord') do begin
    if ((ElementTypeString(c) = 'etSubRecordArray') or (ElementTypeString(c) = 'etSubRecordStruct') or (ElementTypeString(c) = 'etArray')) then
      a := '['+IntToStr(IndexOf(c, e))+']'
    else
      a := Name(e);
    if Result <> '' then Result := a + '\' + Result
    else Result := a;
    e := c;
    c := GetContainer(e);
  end;
end;

function ElementTypeString(e: IInterface): string;
begin               
  Result := '';
  if ElementType(e) = etFile then
    Result := 'etFile'
  else if ElementType(e) = etMainRecord then
    Result := 'etMainRecord'
  else if ElementType(e) = etGroupRecord then
    Result := 'etGroupRecord'
  else if ElementType(e) = etSubRecord then
    Result := 'etSubRecord'
  else if ElementType(e) = etSubRecordStruct then
    Result := 'etSubRecordStruct'
  else if ElementType(e) = etSubRecordArray then
    Result := 'etSubRecordArray'
  else if ElementType(e) = etSubRecordUnion then
    Result := 'etSubRecordUnion'
  else if ElementType(e) = etArray then
    Result := 'etArray'
  else if ElementType(e) = etStruct then
    Result := 'etStruct'
  else if ElementType(e) = etValue then
    Result := 'etValue'
  else if ElementType(e) = etFlag then
    Result := 'etFlag'
  else if ElementType(e) = etStringListTerminator then
    Result := 'etStringListTerminator'
  else if ElementType(e) = etUnion then
    Result := 'etUnion'
  else if ElementType(e) = etStructChapter then
    Result := 'etStructChapter';
end;

function DelimitedTextBetween(var sl: TStringList; first, last: integer): string;
var
  i: integer;
begin
  Result := '';
  for i := first to last do begin
    Result := Result + sl[i];
    if i < last then Result := Result + sl.Delimiter;
  end;
end;

function GetTextIn(str: string; open, close: char): string;
var
  i, openIndex: integer;
  bOpen: boolean;
begin
  Result := '';
  bOpen := false;
  for i := 1 to Length(str) do begin
    if not bOpen and (GetChar(str, i) = open) then begin
      openIndex := i;
      bOpen := true;
    end;
    if bOpen and (GetChar(str, i) = close) then begin
      Result := CopyFromTo(str, openIndex + 1, i - 1);
      break;
    end;
  end;
end;

function GetChar(const s: string; n: integer): char;
begin
  Result := Copy(s, n, 1);
end;

function CopyFromTo(s: string; p1: integer; p2: integer): string;
begin
  Result := '';
  if p1 > p2 then exit; 
  Result := Copy(s, p1, p2 - p1 + 1);
end;

procedure ElementsByMIP(var lst: TList; e: IInterface; ip: string);
var
  xstr: string;
  i, j, index: integer;
  path: TStringList;
  bMult: boolean;
begin
  // replace forward slashes with backslashes
  ip := StringReplace(ip, '/', '\', [rfReplaceAll]);
  
  // prepare path stringlist delimited by backslashes
  path := TStringList.Create;
  path.Delimiter := '\';
  path.StrictDelimiter := true;
  path.DelimitedText := ip;
  
  // traverse path
  bMult := false;
  for i := 0 to Pred(path.count) do begin // if path ip is not empty
    if Pos('[', path[i]) > 0 then begin // if there is an index
      xstr := GetTextIn(path[i], '[', ']'); // get value of index, string or integer

      if xstr = '*' then begin // if index is *, iterate through all possible elements of path
        for j := 0 to Pred(ElementCount(e)) do // for all possible elements of path, index 
          ElementsByMIP(lst, ElementByIndex(e, j), DelimitedTextBetween(path, i + 1, Pred(path.count)));
        bMult := true;
        break;
      end
      else begin
        // e := ElementByPath(e, path[i]);
        ElementsByMIP(lst, ElementByIndex(e, StrToInt(xstr)), DelimitedTextBetween(path, i + 1, Pred(path.count)));
        bMult := true; // Will break writing json if False
        break; 
      end;
    end
    else
      e := ElementByPath(e, path[i]);
  end;
  if not bMult then lst.Add(TObject(e));

  path.Free;
end;

Function PrepareIP(ip: string; path: TStringList): string;
begin
  ip := StringReplace(ip, '/', '\', [rfReplaceAll]);

  path.Delimiter := '\';
  path.StrictDelimiter := true;
  path.DelimitedText := ip;

  Result := ip;
end;

procedure ElementsByMIPWithoutPrepareIP(outputPathList: TList; e: IInterface; inputStringPath: TStringList);
var
  xstr: string; // Index items in path
  i, j, index: integer;
  bMult: boolean;
begin
  // traverse path
  bMult := false;
  for i := 0 to Pred(inputStringPath.Count) do begin // Iterate through path stringlist
    AddMessage('ElementsByMIPWithoutPrepareIP Index: ' + IntToStr(i));
    if Pos('[', inputStringPath[i]) > 0 then begin // if there is an index
      AddMessage('If there is an index block');
      xstr := GetTextIn(inputStringPath[i], '[', ']'); // get value of index
      if xstr = '*' then begin // if index is *, iterate through all possible elements of path
        AddMessage('If index is * block');
        for j := 0 to Pred(ElementCount(e)) do // for all possible elements of path, index 
          AddMessage('Index for all possible elements: ' + IntToStr(j));
          ElementsByMIPWithoutPrepareIP(outputPathList, ElementByIndex(e, j), DelimitedTextBetween(inputStringPath, i + 1, Pred(inputStringPath.Count)));
        bMult := true;
        break;
      end
      else begin
        ElementsByMIPWithoutPrepareIP(outputPathList, ElementByIndex(e, StrToInt(xstr)), DelimitedTextBetween(inputStringPath, i + 1, Pred(inputStringPath.Count)));
        bMult := true; // Will break writing json if False
      end;
      break; 
    end
    else begin
      e := ElementByPath(e, inputStringPath[i]);
    end;
  end;
  if not bMult then outputPathList.Add(TObject(e));
end;

{
  mgeev:
  Uses GetEditValues on each element in a list of elements to
  produce a stringlist of element edit values.  Use with ElementsByMIP.
  
  Example usage:
  lst := TList.Create;
  // setup an arrray in lst with ElementsByMIP
  sl := TStringList.Create;
  mgeev(sl, lst);
}
procedure mgeev(var sl: TStringList; var lst: TList);
var
  i: integer;
  e: IInterface;
begin
  for i := 0 to Pred(lst.Count) do begin
    e := ObjectToElement(lst[i]);
    if Assigned(e) then
      sl.Add(GetEditValue(e))
    else
      sl.Add('');
  end;
end;

function SplitStrToListFunc(str: string; delimiter: string; sStrList: TStringList): TStringList;
begin
  sStrList.Delimiter := delimiter;
  sStrList.StrictDelimiter := true;
  sStrList.DelimitedText := str;
  sStrList := TrimStringList(sStrList);
  Result := sStrList;
end;

function RegexMatch(str: String; regexQuery: String;): boolean;
var
  regexp: TPerlRegEx;
begin
  regexp := TPerlRegEx.Create;
  regexp.Subject := str;
  regexp.RegEx := regexQuery;
  regexp.Options := [];
  regexp.Options := regexp.Options + [preSingleLine];
  regexp.Options := regexp.Options + [preUnGreedy];

  Result := regexp.match;
  regexp.Free;
end;

function StartsWith(str, substr: string;): boolean;
begin
  Result := (Copy(str, 1, Length(substr)) = substr);
end;

function EndsWith(str, substr: string;): boolean;
begin
  Result := (Copy(str, Length(str) - Length(substr) + 1, Length(substr)) = substr);
end;

function IsIndex(str: String): boolean;
begin
  if StartsWith(str, '[') and EndsWith(str, ']') then
    Result := true
  else
    Result := false
end;

function RemoveFromEnd(s1, s2: string): string;
begin
  Result := s1;
  if EndsWith(s1, s2) then
    Result := Copy(s1, 1, Length(s1) - Length(s2));
end;

function RemoveFromStart(s1, s2: string): string;
begin
  Result := s1;
  if StartsWith(s1, s2) then
    Result := Copy(s1, Length(s2) + 1, MaxInt);
end;

function TrimStringList(sStrList: TStringList): TStringList;
begin
  for i := 0 to Pred(sStrList.Count) do begin
    sStrList[i] := Trim(sStrList[i]);
  end;
  Result := sStrList;
end;

function TrimChars(str: string; chars: TStringList): string;
begin
  for i := 0 to Pred(chars.Count) do begin
    str := RemoveFromEnd(RemoveFromStart(str, chars[i]), chars[i]);
  end;
  Result := str;
end;

function ExtractIndexStr(str: String): integer;
var
  charsTrim: TStringList;
begin
  charsTrim := TStringList.Create;
  charsTrim.Add(' ');
  charsTrim.Add('[');
  charsTrim.Add(']');
  
  Result := StrToInt(TrimChars(str, charsTrim));
  charsTrim.Free;
end;

function BoolToStr(b: boolean): string;
begin
  if b then Result := 'True' else Result := 'False';
end;

end.