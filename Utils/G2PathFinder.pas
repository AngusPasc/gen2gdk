//G2PathFinder v1.0
//  Author: Dan
//  url - http://dan.mirgames.ru
//  email - dan.soft.studio@gmail.com
//
//Instructions:
//
//  Globals:
//    var
//      PathFinder: TG2PathFinder;
//      Map: PG2PathMap;
//      WayPoints: TG2PathWayPoints
//
//  Initialization:
//    PathFinder := TG2PathFinder.Create;
//    Map := PathFinder.CreateMap(VarWidth, VarHeight);
//
//  Finalization:
//    PathFinder.FreeMap(Map);
//    PathFinder.Free;
//
//  Path Finding:
//    PathFinder.FindPath(
//      Map,
//      VarStartPosX, VarStartPosY,
//      VarFinishPosX, VarFinishPosY,
//      @WayPoints
//    );
//
//  Properties:
//    TG2PathFinder.Heuristic:
//      Heuristic calculation algorithm
//      phManhattan
//      phDiagonal
//      phDistance
//      phWave (no Heuristic)
//
//    TG2PathFinder.HAmp:
//      An amplifier of heuristic effect
//
//    TG2PathFinder.MoveDiag:
//      Allow the path to go through diagonally
//      located nodes.
//      Example:
//        X - wall
//        S - start
//        F - finish
//        * - waypoints
//
//        MoveDiag = true
//         __________
//        |S*        |
//        |XX*    XXX|
//        |   *   X F|
//        |    ***X* |
//        | XXXXXX*  |
//        |          |
//        |__________|
//
//        MoveDiag = false
//         __________
//        |S**       |
//        |XX*    XXX|
//        |  *    X F|
//        |***    X**|
//        |*XXXXXX** |
//        |********  |
//        |__________|
//
//    TG2PathFinder.WallBend:
//      Works in combination with MoveDiag = true
//      Example:
//
//        MoveDiag = true and WallBend = true
//         __________
//        |S**       |
//        |XX*    XXX|
//        |  *    X F|
//        |**     X *|
//        |*XXXXXX * |
//        |********  |
//        |__________|
//
//
//    TG2PathNode
//      Can be accessed through Map.Nodes[X, Y]
//      TG2PathNode.Cost
//        The cost of travelling through this node,
//        the higher the cost the less likely it is
//        that the path with go through this node,
//        an exception is the cost of 0 which means
//        the node is a solid wall
//
unit G2PathFinder;

{$include ../Gen2.inc}

interface

uses
  Types, Classes, Math;

type
  TG2PathFinder = class;

  TG2PathHeuristic = (
    phManhattan,
    phDiagonal,
    phDistance,
    phWave
  );

  PG2PathNode = ^TG2PathNode;
  TG2PathNode = record
    X: Integer;
    Y: Integer;
    Cost: DWord;
    H: DWord;
    F: DWord;
    Parent: PG2PathNode;
    Open: boolean;
  end;

  TG2PathWayPoint = record
    X, Y: DWord;
  end;

  PG2PathWayPoints = ^TG2PathWayPoints;
  TG2PathWayPoints = array of TG2PathWayPoint;

  PG2Path = ^TG2Path;
  TG2Path = record
    Nodes: array of PG2PathNode;
  end;

  PG2PathMap = ^TG2PathMap;
  TG2PathMap = record
    Nodes: array of array of TG2PathNode;
    CostH: WORD;
    CostV: WORD;
    CostD: WORD;
    BaseCost: WORD;
  end;

  TG2PathFinder = class
  private
    m_LastPath: PG2Path;
    m_HeuristicType: TG2PathHeuristic;
    m_HAmp: single;
    m_MoveDiag: boolean;
    m_WallBend: boolean;
  public
    property Path: PG2Path read m_LastPath write m_LastPath;
    property Heuristic: TG2PathHeuristic read m_HeuristicType write m_HeuristicType;
    property HAmp: single read m_HAmp write m_HAmp;
    property MoveDiag: boolean read m_MoveDiag write m_MoveDiag;
    property WallBend: boolean read m_WallBend write m_WallBend;
    constructor Create;
    destructor Destroy; override;
    function CreateMap(
      const Width, Height: DWord;
      const BaseCost: WORD = 20;
      const CostH: WORD = 10;
      const CostV: WORD = 10;
      const CostD: WORD = 14
    ): PG2PathMap;
    procedure FreeMap(var Map: PG2PathMap);
    function FindPath(
      const Map: PG2PathMap;
      const StartX, StartY, FinishX, FinishY: Integer;
      const WayPoints: PG2PathWayPoints = nil
    ): PG2Path;
  end;
  
implementation

//TG2PathFinder BEGIN
constructor TG2PathFinder.Create;
begin
  inherited Create;
  New(m_LastPath);
  m_HeuristicType := phManhattan;
  m_HAmp := 1;
  m_MoveDiag := true;
  m_WallBend := false;
end;

destructor TG2PathFinder.Destroy;
begin
  SetLength(m_LastPath.Nodes, 0);
  Dispose(m_LastPath);
  inherited Destroy;
end;

function TG2PathFinder.CreateMap(
      const Width, Height: DWord;
      const BaseCost: WORD = 20;
      const CostH: WORD = 10;
      const CostV: WORD = 10;
      const CostD: WORD = 14
    ): PG2PathMap;
var
  i, j: Integer;
begin
  New(Result);
  SetLength(Result^.Nodes, Width, Height);
  Result^.CostH := CostH;
  Result^.CostV := CostV;
  Result^.CostD := CostD;
  Result^.BaseCost := BaseCost;
  for j := 0 to Height - 1 do
  for i := 0 to Width - 1 do
  begin
    Result^.Nodes[i,j].X := i;
    Result^.Nodes[i,j].Y := j;
    Result^.Nodes[i,j].Cost := BaseCost;
    Result^.Nodes[i,j].H := 0;
    Result^.Nodes[i,j].F := 0;
    Result^.Nodes[i,j].Parent := nil;
    Result^.Nodes[i,j].Open := False;
  end;
end;

procedure TG2PathFinder.FreeMap(var Map: PG2PathMap);
begin
  SetLength(Map^.Nodes, 0);
  Dispose(Map);
  Map := nil;
end;

function TG2PathFinder.FindPath(
      const Map: PG2PathMap;
      const StartX, StartY, FinishX, FinishY: Integer;
      const WayPoints: PG2PathWayPoints = nil
    ): PG2Path;
  function GetHeuristic(X, Y: Integer): Integer;
  var
    DX, DY, sMin, sMax: DWord;
  begin
    case m_HeuristicType of
      phManhattan:
      Result := Abs(X - FinishX) * Map^.CostH + Abs(Y - FinishY) * Map^.CostV;
      phDiagonal:
      begin
        DX := Abs(X - FinishX);
        DY := Abs(Y - FinishY);
        sMin := Min(DX, DY);
        sMax := Max(DX, DY);
        if DX < DY then
        Result := sMin * Map^.CostD + (sMax - sMin) * Map^.CostV
        else
        Result := sMin * Map^.CostD + (sMax - sMin) * Map^.CostH;
      end;
      phDistance:
      begin
        DX := Abs(X - FinishX) * Map^.CostH;
        DY := Abs(Y - FinishY) * Map^.CostV;
        Result := Round(Sqrt((DX * DX) + (DY * DY)));
      end;
      else
      Result := 1;
    end;
    if m_HAmp <> 1 then
    Result := Round(Result * m_HAmp);
  end;
var
  i, j: Integer;
  CurNode: PG2PathNode;
  TestNode: PG2PathNode;
  MoveDir: array of TPoint;
  MoveCosts: array[0..7] of Word;
  OpenNodes: TList;
  PathFound: Boolean;
  PathLength: Integer;
  NewCost: DWord;
  NewPos: TPoint;
  AllowNode: Boolean;
  procedure AddOpenNode(Node: PG2PathNode);
  var
    l, h, dif, m: Integer;
  begin
    l := 0;
    h := OpenNodes.Count - 1;
    while l <= h do
    begin
      m := (l + h) div 2;
      dif := PG2PathNode(OpenNodes.Items[m])^.F - Node^.F;
      if dif < 0 then l := m + 1
      else h := m - 1;
    end;
    OpenNodes.Insert(l, Node);
    Node^.Open := true;
  end;
  procedure RemoveOpenNode(Node: PG2PathNode);
  begin
    OpenNodes.Remove(Node);
    Node^.Open := false;
  end;
const
  DiagonalMoveDir: array [0..7] of TPoint = (
    (X: -1; Y: -1), (X: 0; Y: -1), (X: 1; Y: -1), (X: 1; Y: 0),
    (X: 1; Y: 1), (X: 0; Y: 1), (X: -1; Y: 1), (X: -1; Y: 0)
  );
  LinearMoveDir: array [0..3] of TPoint = (
    (X: 0; Y: -1), (X: 1; Y: 0), (X: 0; Y: 1), (X: -1; Y: 0)
  );
begin
  PathFound := False;
  CurNode := nil;
  if (Map^.Nodes[StartX, StartY].Cost = 0)
  or (Map^.Nodes[FinishX, FinishY].Cost = 0) then
  begin
    SetLength(m_LastPath.Nodes, 0);
    if WayPoints <> nil then
    SetLength(WayPoints^, 0);
    Result := m_LastPath;
    Exit;
  end;
  if m_MoveDiag then
  begin
    SetLength(MoveDir, 8);
    Move(DiagonalMoveDir[0], MoveDir[0], SizeOf(TPoint) * 8);
    MoveCosts[0] := Map^.CostD;
    MoveCosts[1] := Map^.CostV;
    MoveCosts[2] := Map^.CostD;
    MoveCosts[3] := Map^.CostH;
    MoveCosts[4] := Map^.CostD;
    MoveCosts[5] := Map^.CostV;
    MoveCosts[6] := Map^.CostD;
    MoveCosts[7] := Map^.CostH;
  end
  else
  begin
    SetLength(MoveDir, 4);
    Move(LinearMoveDir[0], MoveDir[0], SizeOf(TPoint) * 4);
    MoveCosts[0] := Map^.CostV;
    MoveCosts[1] := Map^.CostH;
    MoveCosts[2] := Map^.CostV;
    MoveCosts[3] := Map^.CostH;
  end;
  for j := 0 to High(Map^.Nodes[0]) do
  for i := 0 to High(Map^.Nodes) do
  begin
    Map^.Nodes[i, j].H := 0;
    Map^.Nodes[i, j].F := 0;
    Map^.Nodes[i, j].Parent := nil;
    Map^.Nodes[i, j].Open := False;
  end;
  OpenNodes := TList.Create;
  AddOpenNode(@Map^.Nodes[StartX, StartY]);
  Map^.Nodes[StartX, StartY].H := GetHeuristic(StartX, StartY);
  while (OpenNodes.Count > 0) and not PathFound do
  begin
    CurNode := OpenNodes.Items[0];
    if (CurNode^.X = FinishX) and (CurNode^.Y = FinishY) then
    PathFound := True
    else
    begin
      for i := 0 to High(MoveDir) do
      begin
        NewPos.X := CurNode^.X + MoveDir[i].X;
        NewPos.Y := CurNode^.Y + MoveDir[i].Y;
        if (NewPos.X < 0) or (NewPos.X > High(Map^.Nodes))
        or (NewPos.Y < 0) or (NewPos.Y > High(Map^.Nodes[0])) then
        Continue;
        TestNode := @Map^.Nodes[NewPos.X, NewPos.Y];
        if not m_MoveDiag or not m_WallBend then
        AllowNode := True
        else
        AllowNode := (
          Odd(i)
          or
          (
            not Odd(i)
            and
            (
              (Map^.Nodes[NewPos.X, CurNode^.Y].Cost > 0)
              and
              (Map^.Nodes[CurNode^.X, NewPos.Y].Cost > 0)
            )
          )
        );
        if (TestNode^.Cost > 0) and AllowNode then
        begin
          if (m_HeuristicType <> phWave) and (TestNode^.H = 0) then
          TestNode^.H := GetHeuristic(TestNode^.X, TestNode^.Y);
          NewCost := CurNode^.F  + TestNode^.H + TestNode^.Cost + MoveCosts[i];
          if (TestNode^.F = 0) or (NewCost < TestNode^.F) then
          begin
            TestNode^.F := NewCost;
            TestNode^.Parent := CurNode;
            if not TestNode^.Open then AddOpenNode(TestNode);
          end;
        end;
      end;
    end;
    RemoveOpenNode(CurNode);
  end;
  if PathFound then
  begin
    SetLength(m_LastPath^.Nodes, Length(Map^.Nodes[0]) * Length(Map^.Nodes));
    PathLength := 0;
    while CurNode <> @Map^.Nodes[StartX, StartY] do
    begin
      m_LastPath.Nodes[High(m_LastPath^.Nodes) - PathLength] := CurNode;
      PathLength := PathLength + 1;
      CurNode := CurNode.Parent;
    end;
    m_LastPath.Nodes[High(m_LastPath^.Nodes) - PathLength] := CurNode;
    PathLength := PathLength + 1;
    if PathLength > 0 then
    Move(
      m_LastPath^.Nodes[high(m_LastPath^.Nodes) - (PathLength - 1)],
      m_LastPath^.Nodes[0],
      PathLength * SizeOf(PG2PathNode)
    );
    SetLength(m_LastPath^.Nodes, PathLength);
    if WayPoints <> nil then
    begin
      SetLength(WayPoints^, PathLength);
      for i := 0 to High(m_LastPath^.Nodes) do
      Move(m_LastPath^.Nodes[i]^, WayPoints^[i], sizeof(DWord) * 2);
    end;
  end
  else
  begin
    SetLength(m_LastPath.Nodes, 0);
    if WayPoints <> nil then
    SetLength(WayPoints^, 0);
  end;
  Result := m_LastPath;
  OpenNodes.Free;
end;
//TG2PathFinder END

end.
