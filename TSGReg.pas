{*******************************************************}
{                                                       }
{     ObjectSight Visual Components                     }
{     TopGrid components and editors registration unit  }
{                                                       }
{     Copyright (c) 1999-2005, ObjectSight              }
{                                                       }
{*******************************************************}

unit TSGReg;                

{$INCLUDE TSCmpVer}

interface

uses
    TSGrid, TSDBGrid, TSMask, TSImageList, TSDateTime,
    TSImagelistEditor, {TSDateTimeEditor,}
    {$IFNDEF TSVER_V4PRO} tsHTMLGridProducer, {$ENDIF}
    osColorComboBox, osGridEditor
    {$IFDEF TSVER_V6} , DesignEditors, VCLEditors {$ENDIF},
    osAdvGridEditors, osAdvDbGrid, osEditCalendar;

procedure Register;

implementation

uses
    Classes, {$IFDEF TSVER_V6} DesignIntf, {$ELSE} DsgnIntf, {$ENDIF} Controls;

procedure Register;
begin
    RegisterComponents('TopGrid', [TtsGrid, TosAdvDbGrid, TosLayoutManager]);
    RegisterComponents('TopGrid', [TtsDBGrid]);
    RegisterComponents('TopGrid', [TtsMaskDefs]);
    RegisterComponents('TopGrid', [TtsImageList]);
    RegisterComponents('TopGrid', [TtsDateTimeDef]);
    RegisterComponents('TopGrid', [TosTgDateEdit, TosDbTgDateEdit]);
    //RegisterPropertyEditor(TypeInfo(string), TosDateTimePicker, 'DataField', TosDateTimePickerDataFieldProperty);

    {$IFNDEF TSVER_V4PRO} RegisterComponents('TopGrid', [TtsHTMLGridProducer]); {$ENDIF}
    RegisterComponents('TopGrid', [TosColorComboBox]);

    RegisterPropertyEditor(TypeInfo(string), TtsCol, 'FieldName', TStringProperty);
    //RegisterPropertyEditor(TypeInfo(TShortCutProperty), TtsGrid, 'MemoEditorShortCut', TShortCutProperty);
    RegisterPropertyEditor(TypeInfo(TShortCut), Nil, '', TShortCutProperty);
    
//rh    RegisterPropertyEditor(TypeInfo(TDate), TtsDateTimeDefProps, 'MinDate', TtsDateTimeDateProperty);
//rh    RegisterPropertyEditor(TypeInfo(TDate), TtsDateTimeDefProps, 'MaxDate', TtsDateTimeDateProperty);

    RegisterComponentEditor(TtsBaseGrid, TosGridEditor);
    RegisterPropertyEditor(TypeInfo(TtsImageCollection), TtsImageList, '', TtsImageCollectionEditor);
    RegisterComponentEditor(TtsImageList, TtsImageListEditor);

    RegisterComponentEditor(TosAdvDbGrid, TosAdvGridColumnEditor);
    //RegisterComponentEditor(TtsDateTimeDef, TtsDateTimeEditor);
end;

end.
