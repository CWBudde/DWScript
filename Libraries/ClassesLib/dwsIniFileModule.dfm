object dwsIniFileLib: TdwsIniFileLib
  OldCreateOrder = False
  Height = 150
  Width = 215
  object dwsIniFile: TdwsUnit
    Classes = <
      item
        Name = 'TIniFile'
        Constructors = <
          item
            Name = 'Create'
            Parameters = <
              item
                Name = 'fileName'
                DataType = 'String'
              end>
            OnEval = dwsIniFileClassesTIniFileConstructorsCreateEval
          end
          item
            Name = 'CreateInMemory'
            Parameters = <
              item
                Name = 'content'
                DataType = 'String'
              end>
            OnEval = dwsIniFileClassesTIniFileConstructorsCreateInMemoryEval
          end>
        Methods = <
          item
            Name = 'FileName'
            ResultType = 'String'
            OnEval = dwsIniFileClassesTIniFileMethodsFileNameEval
            Kind = mkFunction
          end
          item
            Name = 'EraseSection'
            Parameters = <
              item
                Name = 'section'
                DataType = 'String'
              end>
            OnEval = dwsIniFileClassesTIniFileMethodsEraseSectionEval
            Kind = mkProcedure
          end
          item
            Name = 'DeleteKey'
            Parameters = <
              item
                Name = 'section'
                DataType = 'String'
              end
              item
                Name = 'name'
                DataType = 'String'
              end>
            OnEval = dwsIniFileClassesTIniFileMethodsDeleteKeyEval
            Kind = mkProcedure
          end
          item
            Name = 'ReadString'
            Parameters = <
              item
                Name = 'section'
                DataType = 'String'
              end
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'default'
                DataType = 'String'
                HasDefaultValue = True
                DefaultValue = ''
              end>
            ResultType = 'String'
            OnEval = dwsIniFileClassesTIniFileMethodsReadStringEval
            Kind = mkFunction
          end
          item
            Name = 'WriteString'
            Parameters = <
              item
                Name = 'section'
                DataType = 'String'
              end
              item
                Name = 'name'
                DataType = 'String'
              end
              item
                Name = 'value'
                DataType = 'String'
              end>
            OnEval = dwsIniFileClassesTIniFileMethodsWriteStringEval
            Kind = mkProcedure
          end
          item
            Name = 'ReadSections'
            ResultType = 'array of String'
            OnEval = dwsIniFileClassesTIniFileMethodsReadSectionsEval
            Kind = mkFunction
          end
          item
            Name = 'ReadSectionNames'
            Parameters = <
              item
                Name = 'section'
                DataType = 'String'
              end>
            ResultType = 'array of String'
            OnEval = dwsIniFileClassesTIniFileMethodsReadSectionNamesEval
            Kind = mkFunction
          end
          item
            Name = 'GetEncoding'
            ResultType = 'String'
            OnEval = dwsIniFileClassesTIniFileMethodsGetEncodingEval
            Kind = mkFunction
          end
          item
            Name = 'SetEncoding'
            Parameters = <
              item
                Name = 'n'
                DataType = 'String'
              end>
            OnEval = dwsIniFileClassesTIniFileMethodsSetEncodingEval
            Kind = mkProcedure
          end
          item
            Name = 'ToString'
            ResultType = 'String'
            OnEval = dwsIniFileClassesTIniFileMethodsToStringEval
            Kind = mkFunction
          end>
        Properties = <
          item
            Name = 'Encoding'
            DataType = 'String'
            ReadAccess = 'GetEncoding'
            WriteAccess = 'SetEncoding'
          end>
        OnCleanUp = dwsIniFileClassesTIniFileCleanUp
      end>
    UnitName = 'System.IniFiles'
    StaticSymbols = True
    Left = 72
    Top = 32
  end
end
