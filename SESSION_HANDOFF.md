# SESSION_HANDOFF

Состояние на 2026-08-17.

Контракт, инварианты и соглашения — в `AGENTS.md`. Здесь только состояние.

## Сделано

- `R/conditions.R` — `bq_abort()`, типизированные условия.
- `R/bq-data.R` — `as_bq_data()`, конструкторы `bq_data` и реестра.
- `R/registry.R` — согласование реестра со столбцами и монотонная выдача
  идентификаторов через внутренний `next_var_number`; удалённый `var_id`
  никогда не переиспользуется.
- `R/dplyr-methods.R` — сохранение метаданных через dplyr и base replacement;
  `$<-`, `[[<-` и `[<-` выдают ID новым столбцам и сбрасывают зависимые от
  значений метаданные переписанных. `group_by()`, `rowwise()` и
  `tibble::add_column()` отвергаются явно.
- `R/variables.R` — `variables()`, чтение реестра.
- `R/variable-levels.R` — `variable_levels()`, чтение плоского реестра уровней.
- `R/print.R` — заголовок со сводкой по ролям.
- `R/infer-type.R` — `infer_type()`, вывод аналитического типа из вектора.
  Внутренний `infer_type_metadata()` одновременно возвращает event, его
  источник и объявленный порядок ordinal levels; `max_levels` проверяется как
  положительный целый скаляр.
- `R/infer-types.R` — `infer_types()`, применение вывода к неразмеченным
  столбцам без перезаписи уже принятого решения.
- `R/type-constructors.R` — `continuous()`, `count()`, `binary(event)`,
  `ordinal(levels)` и `nominal(reference)`, декларативные спецификации класса
  `bq_type`.
- `R/set-type.R` — `set_type()`, запись явной спецификации одного столбца в
  плоские реестры с проверкой event, reference, ordinal levels и внутренней
  согласованности specification.
- `R/set-role.R` — `set_role()`, установка одной из ролей `outcome`,
  `predictor`, `group`, `id` для одного или нескольких столбцов, и удобная
  обёртки `set_outcome()` и `set_predictor()`.
- `R/set-format.R` — `set_unit()` и `set_rounding()`, запись единиц измерения и
  политики представления (`decimal` / `significant` + `digits`) без изменения
  исходных чисел.
- `R/apply-dictionary.R` — `apply_dictionary()`, применение основного плоского
  словаря по имени и отдельного ordinal level dictionary с приоритетом над
  inferred/default и защитой explicit.
- `R/selectors.R` — внутренний `resolve_variables()`, немедленно связывающий
  tidyselect-выбор со стабильными `var_id`; на него переведены `set_type()` и
  `set_role()` с его обёртками, а также `infer_types()`.
- `R/plan-summary.R` — `plan_summary()`, минимальный план с анализируемыми
  переменными, group, strata и осями raw Overall; компактный print показывает
  зарегистрированные статистики и их назначения, но не вложенные данные и
  функции. План также содержит плоские реестры `display_rules` (`rule_id`,
  `kind`, `max_n`, `display_statistics`) и `display_rule_assignments`
  (`rule_id`, `var_id`) со счётчиком `next_display_rule_number`.
- `R/continuous-statistic.R` — `continuous_statistic()`, декларация custom raw
  функции с one-row data-frame prototype и пользовательской missing policy.
- `R/add-statistic.R` — `add_statistic()`, регистрация specification, её
  компонентов, назначений `var_id` и исполняемой функции в summary plan.
- `R/preflight.R` — generic `preflight()` и метод summary-плана; повреждённая
  структура плана отвергается, а аналитические проблемы возвращаются плоской
  таблицей diagnostics с адресацией по `var_id`, `statistic_id`, `rule_id` и
  `cell_id`.
  Помимо типов, statistic и format policy, проверяются пропуски на осях
  дизайна и пустые leaf-ячейки; последние две проверки дают warning и не
  блокируют выполнение. `enumerate_values` пока совместим только с continuous;
  несовместимость возвращается как блокирующая `incompatible_display_rule`,
  но не дублируется для missing/unknown type. Результат preflight содержит
  скомпилированные `cells`, `cell_axes` и `cell_rows`.
- `R/compile-cells.R` — внутренний `compile_summary_cells()`, нормализующий
  leaf и raw Overall в три плоских реестра: `cells`, `cell_axes`, `cell_rows`.
  Factor использует объявленные levels, включая пустые комбинации; NA остаётся
  явной ячейкой; multiple strata сворачиваются общей осью Overall.
- `R/enumerate-values.R` — `enumerate_values(max_n = 2L,
  display_statistics = FALSE)`, декларативное display rule для перечисления
  исходных непропущенных значений в малых summary-ячейках. При
  `display_statistics = TRUE` представление одновременно сохраняет статистики;
  набор фактически вычисляемых статистик правило никогда не меняет.
- `R/add-display-rule.R` — `add_display_rule()`, регистрация display rule и
  его назначений summary-переменным по стабильным `var_id`; один specification
  можно назначить нескольким переменным, но у переменной может быть только одно
  правило.
- `R/run-analysis.R` — generic `run_analysis()` и summary-метод. Перед
  вычислением выполняется preflight-gate; blocking diagnostics становятся
  `bq_error_preflight` с приложенным preflight-объектом. Raw continuous engine
  передаёт custom-функции исходный вектор с NA и в исходном порядке для каждой
  ячейки, включая empty и Overall. Результат `bq_result_summary` хранит plan,
  diagnostics, cell registries, системные `sample_sizes` (`n`, `n_missing`) и
  длинный `estimates` без округления. Runtime error и изменение one-row схемы
  останавливают расчёт как `bq_error_statistic_runtime` или
  `bq_error_statistic_schema`, с ID ячейки, переменной и статистики.
- `R/prepare-presentation.R` — generic `prepare_presentation()` и
  summary-метод. Вычисления не меняются: создаются `display_cells`
  (`cell_id`, `var_id`, `status`, `show_statistics`, `show_values`, `rule_id`)
  и raw `display_values` (`cell_id`, `var_id`, `position`, `value`). Status
  различает `observed`, `all_missing` и `empty`; для observed-ячеек с
  `n <= max_n` флаги позволяют показывать перечисление вместо статистик или
  одновременно с ними. NA исключаются, порядок и повторы, включая raw Overall,
  сохраняются; числа ещё не форматируются.
- К `bq_data` добавлен плоский реестр уровней `var_id`, `value`, `position`;
  dplyr сохраняет его, удаляет строки исчезнувших переменных и очищает уровни
  переписанного через `mutate()` столбца.

Реестр сейчас: `var_id`, `name`, `label`, `role`, `type`, `event`,
`event_source`, `reference`, `type_source`, `unit`, `rounding`, `digits`.
Экспортируются конструктор данных и accessor, конструкторы типов, явные
сеттеры ролей и типов, `infer_type()` и `infer_types()`.

762 теста, `R CMD check` — Status: OK. Коммиты `d4650bc`, `d0243f4`,
`6c9e384` запушены в `origin/main`; локальная ветка содержит более новые
коммиты.

## Следующий шаг

Спроектировать форматирование `display_values`: decimal/significant rounding,
trailing zeros, separator и missing/empty tokens. Unit не должна повторяться у
каждого значения и остаётся метаданными переменной для будущей подписи строки.
Отдельно решить component-level format metadata для custom statistics: нельзя
автоматически применять variable rounding к компонентам вроде `observed`, но
для `mean`/`median` такая связь нужна.

## Известные пробелы

- `bind_rows()` и joins берут реестр **первого** аргумента; метаданные второго
  игнорируются молча. Слияния реестров с выявлением конфликтов нет.
- Дубликаты имён столбцов отвергает `tibble::as_tibble()` своей ошибкой, а не
  нашей `bq_error_*`. Единообразия типизации ошибок тут нет.
- Порог `max_levels = 20` в `infer_type()` произволен.
- Кодировка 1/2 (частая в выгрузках SPSS) не распознаётся как двухуровневая.
- `haven_labelled` не обрабатывается: `labelled` в зависимости не входит.
