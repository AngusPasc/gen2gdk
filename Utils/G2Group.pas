//G2Group v1.0
unit G2Group;

{$include ../Gen2.inc}

interface

uses
  Classes,
  Gen2;

type
  TG2Group = class;
  TG2GroupItem = class;

  TG2Group = class
  strict private
    m_List: TList;
    m_NewItems: TG2List;
    m_Updating: Boolean;
    function GetItem(const Index: Integer): TG2GroupItem; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    procedure SetItem(const Index: Integer; const Value: TG2GroupItem);{$IFDEF G2_USE_INLINE} inline; {$ENDIF}
    function GetCount: Integer; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create;
    destructor Destroy; override;
    property Items[const Index: Integer]: TG2GroupItem read GetItem write SetItem; default;
    property Count: Integer read GetCount;
    procedure Render(const Pass: Integer = 0);
    procedure Update;
    procedure Add(const Item: TG2GroupItem);
    procedure Remove(const Item: TG2GroupItem);
    procedure Delete(const Index: Integer);
    procedure FreeItems;
  end;

  TG2GroupItem = class
  strict private
    m_Group: TG2Group;
    m_Dead: Boolean;
    m_Sort: Integer;
    procedure SetSort(const Value: Integer); {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  public
    constructor Create(const Group: TG2Group); virtual;
    destructor Destroy; override;
    property Dead: Boolean read m_Dead write m_Dead;
    property Sort: Integer read m_Sort write SetSort;
    property Group: TG2Group read m_Group write m_Group;
    procedure Render(const Pass: Integer = 0); virtual;
    procedure Update; virtual;
    procedure Die; {$IFDEF G2_USE_INLINE} inline; {$ENDIF}
  end;

implementation

//TG2Group BEGIN
constructor TG2Group.Create;
begin
  inherited Create;
  m_List := TList.Create;
  m_NewItems := TG2List.Create;
  m_Updating := False;
end;

destructor TG2Group.Destroy;
begin
  m_NewItems.Free;
  m_List.Free;
  inherited Destroy;
end;

function TG2Group.GetItem(const Index: Integer): TG2GroupItem;
begin
  Result := TG2GroupItem(m_List[Index]);
end;

procedure TG2Group.SetItem(const Index: Integer; const Value: TG2GroupItem);
begin
  m_List[Index] := Value;
end;

function TG2Group.GetCount: Integer;
begin
  Result := m_List.Count;
end;

procedure TG2Group.Render(const Pass: Integer = 0);
var
  i: Integer;
begin
  for i := 0 to m_List.Count - 1 do
  TG2GroupItem(m_List[i]).Render(Pass);
end;

procedure TG2Group.Update;
var
  i, j: Integer;
begin
  m_Updating := True;
  i := 0;
  j := m_List.Count - 1;
  while i <= j do
  begin
    TG2GroupItem(m_List[i]).Update;
    if TG2GroupItem(m_List[i]).Dead then
    begin
      TG2GroupItem(m_List[i]).Destroy;
      Delete(i);
      Dec(j);
    end
    else
    Inc(i);
  end;
  m_Updating := False;
  if m_NewItems.Count > 0 then
  begin
    for i := 0 to m_NewItems.Count - 1 do
    Add(TG2GroupItem(m_NewItems[i]));
    m_NewItems.Clear;
  end;
end;

procedure TG2Group.Add(const Item: TG2GroupItem);
var
  l, h, dif, m: Integer;
begin
  if m_Updating then
  m_NewItems.Add(Item)
  else
  begin
    Item.Group := Self;
    l := 0;
    h := m_List.Count - 1;
    while l <= h do
    begin
      m := (l + h) div 2;
      dif := TG2GroupItem(m_List[m]).Sort - Item.Sort;
      if dif < 0 then l := m + 1
      else h := m - 1;
    end;
    m_List.Insert(l, Item);
  end;
end;

procedure TG2Group.Remove(const Item: TG2GroupItem);
begin
  m_List.Remove(Item);
end;

procedure TG2Group.Delete(const Index: Integer);
begin
  m_List.Delete(Index);
end;

procedure TG2Group.FreeItems;
begin
  while m_List.Count > 0 do
  begin
    TG2GroupItem(m_List[0]).Free;
    m_List.Delete(0);
  end;
end;
//TG2Group END

//TG2GroupItem BEGIN
constructor TG2GroupItem.Create(const Group: TG2Group);
begin
  inherited Create;
  m_Dead := False;
  m_Sort := 0;
  m_Group := Group;
  Group.Add(Self);
end;

destructor TG2GroupItem.Destroy;
begin
  inherited Destroy;
end;

procedure TG2GroupItem.SetSort(const Value: Integer);
begin
  m_Sort := Value;
  m_Group.Remove(Self);
  m_Group.Add(Self);
end;

procedure TG2GroupItem.Render(const Pass: Integer = 0);
begin

end;

procedure TG2GroupItem.Update;
begin

end;

procedure TG2GroupItem.Die;
begin
  m_Dead := True;
end;
//TG2GroupItem END

end.