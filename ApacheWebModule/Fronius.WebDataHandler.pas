unit Fronius.WebDataHandler;

// ###########################################
// #### Handling of the fronius data requests
// ###########################################

interface

uses SysUtils, Classes, Fronius.ActionsBase,
    {$IFDEF FPC} fpWeb, HTTPDefs {$ELSE}  Web.HTTPApp {$ENDIF},
    Fronius.DBIntf;

type
  TFroniusDataPathHandler = class(TWebActionBase)
  private
    fDB : IFroniusDBAdapter;

    procedure HandleBarData( Request : TWebRequest; Response : TWebResponse );
    procedure HandleDaySample( Request : TWebRequest; Response : TWebResponse );
    procedure HandleCurSample( Request : TWebRequest; Response : TWebResponse );

  protected
    procedure Handler(Sender: TObject; Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  public
    constructor Create(ref : {$IFDEF FPC} TFPWebModule {$ELSE} TWebModule {$ENDIF}; dbHandler : IFroniusDBAdapter );
  end;

implementation

uses Fronius.Sessions, Fronius.Globals, math;

type
  TFroniusWebDataType = (wdCurSample, wdDaySample, wdBarSample);

const cWebDataActionPaths : Array[wdCurSample..wdBarSample] of string =
                      ('/cursamp',
                       '/daysamples',
                       '/bars');

{ TFroniusDataPathHandler }

constructor TFroniusDataPathHandler.Create(ref : {$IFDEF FPC} TFPWebModule {$ELSE} TWebModule {$ENDIF};
  dbHandler: IFroniusDBAdapter);
var item : {$IFDEF FPC} TFPWebAction {$ELSE} TWebActionItem {$ENDIF};
    iter : TFroniusWebDataType;
begin
     inherited Create;

     fDB := dbHandler;

     // ###########################################
     // #### Add handler
     for iter := wdCurSample to wdBarSample do
     begin
          item := ref.Actions.Add;
          {$IFDEF FPC}
          item.Name := Copy(cWebDataActionPaths[iter], 2, $FFFF );
          item.OnRequest := @Handler;
          {$ELSE}
          item.Enabled := True;
          item.PathInfo := cWebDataActionPaths[iter];
          item.OnAction := Handler;
          {$ENDIF}
     end;
end;

procedure TFroniusDataPathHandler.HandleBarData(Request: TWebRequest;
  Response: TWebResponse);
var fromDate, toDate : TDateTime;
    aYear, aDay, aMonth : word;
    actMon, actYear : word;
    barType : integer;
    buf : TStringBuilder;
    startEnProd : double;
    startEnCons : double;
    startEnGrid : double;
    startEnToGrid : double;
    lastEnProd : double;
    lastEnCons : double;
    lastEnGrid : double;
    lastEnToGrid : double;
begin
     // ###########################################
     // #### Return the day/month/year statistics as "bars"


     // ###########################################
     // #### Get params
     // bartype = 0, 1, 2, 3 (day/month/year overall accumulation)
     // fromDay = 1-31 (default = 1)
     // fromMon = 1-12 (default = 1)
     // from Year = 2023 (default = 2023)
     // toDay = 1-31 or 0 for last day in month (default = 0 for last day)
     // toMon = 1-12 (default = 12)
     // toYear = 2023  (default = fromYear)

     barType := GetIntParam(Request, 'barType', 0);

     if not barType in [0, 1, 2, 3] then
     begin
          Response.Content := '{"error":0, "msg":"Bad type parameter"}';
          exit;
     end;

     aYear := word(getIntParam(Request, 'fromYear', CurrentYear));
     aMonth := word(getIntParam(Request, 'fromMon', 1));
     aDay := word(getIntParam(Request, 'fromDay', 1));

     if not TryEncodeDate(aYear, aMonth, aDay, fromDate) then
     begin
          Response.Content := '{"error":1, "msg":"Bad date format"}';
          exit;
     end;

     actMon := Max(1, aMonth) - 1;
     actYear := Max(1, aYear) - 1;

     aYear := word(getIntParam(Request, 'toYear', aYear));
     aMonth := word(getIntParam(Request, 'toMon', 12));
     aDay := word(getIntParam(Request, 'fromDay', 0));
     if (aDay = 0) and (aMonth >= 1) and (aMonth <= 12) then
        aDay := MonthDays[IsLeapYear(aYear), aMonth];

     if not TryEncodeDate(aYear, aMonth, aDay, toDate) then
     begin
          Response.Content := '{"error":1, "msg":"Bad date format"}';
          exit;
     end;

     if fromDate >= toDate then
     begin
          Response.Content := '{"error":2, "msg":"Invalid interval"}';
          exit;
     end;


     // ###########################################
     // #### Collect data
     try
        if barType <= 2 then
        begin
             buf := TStringBuilder.Create(Format('{"fromDate":"%s"', [DateToStr(fromDate, fmt)]));
             DecodeDate(fromDate, aYear, aMonth, aDay);
             buf.Append(Format(',"fromDi":{"year":%d,"month":%d,"day":%d}', [aYear, aMonth, aDay]));

             buf.Append(Format(',"toDate":"%s"', [DateToStr(toDate, fmt)]));
             DecodeDate(toDate, aYear, aMonth, aDay);
             buf.Append(Format(',"toDi":{"year":%d,"month":%d,"day":%d}', [aYear, aMonth, aDay]));
             buf.Append(',"bartype":' + barType.ToString);
             buf.Append(',"data":[');
             try
                // ###########################################
                // #### Daily bar graph data -> best for one month!
                if barType = 0 then
                begin
                     fDB.SetSQLText('Select * from DAYSTATS where SAMPLETIME>=:t1 and SAMPLETIME<=:t2');
                     fDB.SetParamDT('t1', fromDate);
                     fDB.SetParamDT('t2', toDate);
                     fDB.ExecQuery;

                     while not fDB.Eof do
                     begin
                          DecodeDate(fDB.GetResDT('SampleTime', 0), aYear, aMonth, aDay);

                          buf.Append( Format( '{"sdi":{"year":%d,"month":%d,"day":%d},"enprod":%.1f,"enconsume":%.1f,"engrid":%.1f,"entogrid":%.1f}',
                                            [ aYear, aMonth, aDay,
                                              fDB.GetRes('enproduced', 0.01) - fDB.GetRes('beginproduced', 0.01),
                                              fDB.GetRes('enconsume', 0.01) - fDB.GetRes('beginconsume', 0.01),
                                              fDB.GetRes('engrid', 0.01) - fDB.GetRes('begingrid', 0.01),
                                              fDB.GetRes('entogrid', 0.01) - fDB.GetRes('begintogrid', 0.01)
                                            ], fmt ) );

                          fDB.Next;

                          if not fDB.Eof then
                             buf.Append(',');
                     end;

                     fDB.Close;
                     buf.Append(']}');
                end
                // month and year is a bit different:
                // samples are at a day basis so we need the first and last sample
                // of a month/year -> returned is the difference between start and end of a month/year
                // It needs to be determined if selecting the first and last sample would be faster than
                // do the select once and iterate through the elements.
                else if barType in [1, 2] then
                begin
                     startEnProd := 0;
                     startEnCons := 0;
                     startEnGrid := 0;
                     startEnToGrid := 0;
                     lastEnProd := 0;
                     lastEnCons := 0;
                     lastEnGrid := 0;
                     lastEnToGrid := 0;

                     fDB.SetSQLText('Select * from DAYSTATS where SAMPLETIME>=:t1 and SAMPLETIME<=:t2 order by sampletime');
                     fDB.SetParamDT('t1', fromDate);
                     fDB.SetParamDT('t2', toDate);
                     fDB.ExecQuery;

                     if barType = 2 then
                        actMon := 0;

                     while not fDB.Eof do
                     begin
                          DecodeDate(fDB.GetResDT('SampleTime', 0.0), aYear, aMonth, aDay);

                          // check borders -> if crossed produce result
                          if ( (barType = 1) and (aMonth <> actMon) ) or
                             ( (barType = 2) and (aYear <> actYear) ) then
                          begin
                               if startEnProd <> 0 then
                               begin
                                    if buf.Chars[ buf.Length - 1] = '}' then
                                       buf.Append(',');
                                    buf.Append( Format( '{"sdi":{"year":%d,"month":%d,"day":%d},"enprod":%.1f,"enconsume":%.1f,"engrid":%.1f,"entogrid":%.1f}',
                                            [ actYear, actMon, 1,
                                              lastEnProd - startEnProd,
                                              lastEnCons - startEnCons,
                                              lastEnGrid - startEnGrid,
                                              lastEnToGrid - startEnToGrid
                                            ], fmt ) );
                               end;

                               startEnProd := fDB.GetRes('beginproduced', 0.0);
                               startEnCons := fDB.GetRes('beginconsume', 0.0);
                               startEnGrid := fDB.GetRes('begingrid', 0.0);
                               startEnToGrid := fDB.GetRes('begintogrid', 0.0);
                               actMon := aMonth;
                               actYear := aYear;
                          end;

                          lastEnProd := fDB.GetRes('enproduced', 0.0);
                          lastEnCons := fDB.GetRes('enconsume', 0.0);
                          lastEnGrid := fDB.GetRes('engrid', 0.0);
                          lastEnToGrid := fDB.GetRes('entogrid', 0.0);

                          fDB.Next;
                     end;

                     // last element handling:
                     if (startEnProd <> 0) and (lastEnProd <> startEnProd) then
                     begin
                          if buf.Chars[ buf.Length - 1] = '}' then
                             buf.Append(',');
                          buf.Append( Format( '{"sdi":{"year":%d,"month":%d,"day":%d},"enprod":%.1f,"enconsume":%.1f,"engrid":%.1f,"entogrid":%.1f}',
                                  [ actYear, actMon, 1,
                                    lastEnProd - startEnProd,
                                    lastEnCons - startEnCons,
                                    lastEnGrid - startEnGrid,
                                    lastEnToGrid - startEnToGrid
                                  ], fmt ) );
                     end;


                     fDB.Close;
                     buf.Append(']}');
                end;

                Response.Content := buf.ToString;
             finally
                    buf.Free;
             end;
        end
        else
        begin
             // get last element in the db -> difference is the recorded energy
             fDB.SetSQLText('select first 1 * from Daystats');
             fDB.ExecQuery;

             if not fDB.Eof then
             begin
                  fromDate := fDB.GetResDT('SampleTime', 0);
                  buf := TStringBuilder.Create(Format('{"fromDate":"%s"', [DateToStr(fromDate, fmt)]));
                  DecodeDate(fromDate, aYear, aMonth, aDay);
                  buf.Append(Format(',"fromDi":{"year":%d,"month":%d,"day":%d}', [aYear, aMonth, aDay]));

                  startEnProd := fDB.GetRes('beginproduced', 0.0);
                  startEnCons := fDB.GetRes('beginconsume', 0.0);
                  startEnGrid := fDB.GetRes('begingrid', 0.0);
                  startEnToGrid := fDB.GetRes('begintogrid', 0.0);


                  fDB.Close;
                  fDB.SetSQLText('select skip ((select count(*) - 1 from DAYSTATS)) * from Daystats');
                  fDB.ExecQuery;

                  buf.Append(Format(',"toDate":"%s"', [DateToStr(toDate, fmt)]));
                  DecodeDate(toDate, aYear, aMonth, aDay);
                  buf.Append(Format(',"toDi":{"year":%d,"month":%d,"day":%d}', [aYear, aMonth, aDay]));
                  buf.Append(',"bartype":' + barType.ToString);

                  buf.Append(',"data":[');

                  DecodeDate(fDB.GetResDT('SampleTime', 0), aYear, aMonth, aDay);

                  buf.Append( Format( '{"sdi":{"year":%d,"month":%d,"day":%d},"enprod":%.1f,"enconsume":%.1f,"engrid":%.1f,"entogrid":%.1f}',
                                    [ aYear, aMonth, aDay,
                                      fDB.GetRes('enproduced', 0.0) - startEnProd,
                                      fDB.GetRes('enconsume', 0.0) - startEnCons,
                                      fDB.GetRes('engrid', 0.0) - startEnGrid,
                                      fDB.GetRes('entogrid', 0.0) - startEnToGrid
                                    ], fmt ) );
                  buf.Append(']}');

                  Response.Content := buf.ToString;
                  buf.Free;
             end
             else
                 Response.Content := '{"error":3,"msg":"No data recorded"}';
        end;
     finally
            fDB.Commit;
     end;
end;


procedure TFroniusDataPathHandler.HandleCurSample(Request: TWebRequest;
  Response: TWebResponse);
begin
     // ###########################################
     // #### now fetch the last sample from the database
     // -> normaly this one is updated every 2 seconds
     fDB.SetSQLText('select first 1 * from daysamples where sampletime>:t1 order by sampletime descending');
     fDB.SetParamDT('t1', now - 30/SecsPerDay); // sample as max as 30 seconds in the past
     fDB.ExecQuery;

     if not fDB.Eof then
     begin
          Response.Content := Format('{"t":"%s","tf":%.6f,"PPV":%.1f,"PL":%.1f,"PG":%.1f,"RA":%.2f,"RSC":%.2f}',
                            [
                             FormatDateTime(fmt.LongDateFormat + ' ' + fmt.LongTimeFormat, fDB.GetResDT('SampleTime', 0)),
                             fDB.GetResDT('SampleTime', 0.0),
                             fDB.GetRes('POWERPV', 0.0001),
                             fDB.GetRes('POWERLOAD', 0.0001),
                             fDB.GetRes('POWERGRID', 0.0001),
                             fDB.GetRes('RELAUTONOMY', 0.0001),
                             fDB.GetRes('RELSELFCONSUMPTION', 0.0001)
                            ], fmt);
     end
     else
         Response.Content := ' {"error":1,"msg":"Database not updated in time"}';

     fDB.Close;
     fDB.Commit;
end;

procedure TFroniusDataPathHandler.HandleDaySample(Request: TWebRequest;
  Response: TWebResponse);
var s : string;
    dt : TDateTime;
    sBuf : TStringBuilder;
begin
     // ###########################################
     // #### extract params
     s := getStringParam(Request, 'date', '');
     if not TryStrToDate(s, dt, fmt) then
        dt := now;

     // ###########################################
     // #### now fetch data from the db
     sBuf := TStringBuilder.Create( Format( '{"date":"%s","data":[', [FormatDateTime(fmt.ShortDateFormat, dt, fmt)] ) );
     try
        try
           fDB.SetSQLText('Select * from Samples where sampletime>=:t1 and sampletime<:t2');
           fDB.SetParamDT('t1', dt);
           fDB.SetParamDT('t2', dt + 1);
           fDB.ExecQuery;

           while not fDB.Eof do
           begin
                // result rounded to Watt
                // t is seconds of day
                sBuf.Append( Format('{"t":%d,"PPV":%d,"PL":%d,"PG":%d,"RA":%.2f,"RSC":%.2f}',
                                  [
                                   Round(SecsPerDay*Frac(fDB.GetResDT('SampleTime', 0.0))),
                                   Round(fDB.GetRes('POWERPV', 0.0)),
                                   Round(fDB.GetRes('POWERLOAD', 0.0)),
                                   Round(fDB.GetRes('POWERGRID', 0.0)),
                                   fDB.GetRes('RELAUTONOMY', 0.0),
                                   fDB.GetRes('RELSELFCONSUMPTION', 0.0)
                                  ], fmt) );

                fDB.Next;
                if not fDB.Eof then
                   sBuf.Append(',');
           end;

           sBuf.Append(']}');
           fDB.Close;
        finally
               fDB.Commit;
        end;

        Response.Content := sBuf.ToString;
     finally
            sBuf.Free;
     end;
end;

procedure TFroniusDataPathHandler.Handler(Sender: TObject; Request: TWebRequest;
  Response: TWebResponse; var Handled: Boolean);
var aPath : string;
    sessObj : IFroniusSession;
begin
     Handled := True;

     // ###########################################
     // #### Init headers
     InitResponse(Response);

     if not Sessions.HasValidSession(request, sessObj) then
     begin
          response.Content := '{"error":-2,"msg":"Not logged in - need to renew the session"}';
          exit;
     end;

     if not fDB.IsConnected then
     begin
          Response.Content := '{"error":-1,"msg":"No databse connected"}';
          exit;
     end;

     // ###########################################
     // #### Path handler
     aPath := Request.PathInfo;

     if SameText(aPath, cWebDataActionPaths[wdCurSample])
     then
         HandleCurSample(Request, Response)
     else if SameText(aPath, cWebDataActionPaths[wdDaySample])
     then
         HandleDaySample(Request, Response)
     else if SameText(aPath, cWebDataActionPaths[wdBarSample])
     then
         HandleBarData(Request, Response)
     else
         raise Exception.Create('This path is not supported');

end;

end.
