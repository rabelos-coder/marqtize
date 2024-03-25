'use client'

import { useTranslations } from 'next-intl'
import { ReactNode, useEffect, useState } from 'react'
import DataTable, { createTheme, TableProps } from 'react-data-table-component'

import { APP_PAGINATION_ROWS, APP_PAGINATION_SHOW_ALL } from '@/environment'
import { useAppSelector } from '@/hooks'

import { SpinnerLine, SpinnerLineProps } from './SpinnerLine'

export interface SelectChangeState<T> {
  allSelected: boolean
  selectedCount: number
  selectedRows: T[]
}

createTheme(
  'tm-dark',
  {
    text: {
      primary: '#e6e6e6',
      secondary: '#e2e2e2',
    },
    background: {
      default: '#272932',
    },
    context: {
      background: '#2a2b32',
      text: '#fff',
    },
    divider: {
      default: '#2d2d31',
    },
    action: {
      button: 'rgba(49, 61, 74, 1)',
      hover: 'rgba(49, 61, 74, 1)',
      disabled: 'rgba(49, 61, 74, 0.7)',
    },
    highlightOnHover: {
      default: 'rgba(49, 61, 74, 1)',
    },
  },
  'dark'
)

createTheme(
  'tm-light',
  {
    context: {
      background: '#EFF4FB',
    },
    highlightOnHover: {
      default: '#EFF4FB',
    },
    ThemeStriped: {
      default: '#EFF4FB',
    },
  },
  'light'
)

type CustomTableProps = {
  selectAllRowsItem?: boolean
  spinner?: SpinnerLineProps
  noDataComponentText?: string | ReactNode
}

export default function Table<T>(props: TableProps<T> & CustomTableProps) {
  const t = useTranslations()
  const [dataTableTheme, setDataTableTheme] = useState('tm-light')
  const { theme } = useAppSelector((state) => state.theme)

  useEffect(() => {
    setDataTableTheme(theme === 'light' ? 'tm-light' : 'tm-dark')
    if (props.subHeader) {
      document
        .querySelector('.rdt_TableHeader')
        ?.classList.add('rdt_TableHeaderWithSubHeader')
    }
  }, [theme, props.subHeader])

  return (
    <div className="dataTables_wrapper">
      <DataTable
        title={props.title ?? <></>}
        columns={props.columns}
        data={props.data}
        theme={dataTableTheme}
        disabled={props.disabled ?? false}
        responsive={true}
        striped={false}
        progressPending={props.progressPending ?? false}
        progressComponent={
          props.progressComponent ?? (
            <div className="my-4">
              <SpinnerLine
                type={props.spinner?.type ?? 'border'}
                color={props.spinner?.color ?? 'default'}
                className={`py-3 ${props.spinner?.className ?? ''}`}
              />
            </div>
          )
        }
        className={props.className ?? ''}
        paginationServerOptions={props.paginationServerOptions}
        paginationResetDefaultPage={props.paginationResetDefaultPage}
        paginationDefaultPage={props.paginationDefaultPage ?? 1}
        defaultSortAsc={props.defaultSortAsc ?? true}
        defaultSortFieldId={props.defaultSortFieldId}
        fixedHeader={props.fixedHeader ?? false}
        selectableRows={props.selectableRows}
        contextActions={props.contextActions}
        contextMessage={{
          singular: t('record'),
          plural: t('records'),
          message: `${t('selected')}.`,
        }}
        persistTableHead={props.persistTableHead ?? false}
        fixedHeaderScrollHeight={props.fixedHeaderScrollHeight}
        highlightOnHover={props.highlightOnHover ?? true}
        pointerOnHover={props.pointerOnHover}
        onSelectedRowsChange={props.onSelectedRowsChange}
        clearSelectedRows={props.clearSelectedRows}
        expandableRows={props.expandableRows}
        expandableRowsComponent={props.expandableRowsComponent}
        onSort={props.onSort}
        pagination={props.pagination}
        paginationComponentOptions={{
          rowsPerPageText: t('rowsPerPageText'),
          rangeSeparatorText: t('of'),
          selectAllRowsItem: props.selectAllRowsItem ?? APP_PAGINATION_SHOW_ALL,
          selectAllRowsItemText: t('all'),
        }}
        onRowClicked={props.onRowClicked}
        onRowExpandToggled={props.onRowExpandToggled}
        selectableRowDisabled={props.selectableRowDisabled}
        selectableRowsVisibleOnly={props.selectableRowsVisibleOnly}
        onRowDoubleClicked={
          props.onRowDoubleClicked ? props.onRowDoubleClicked : () => {}
        }
        paginationRowsPerPageOptions={APP_PAGINATION_ROWS}
        paginationServer={props.paginationServer}
        paginationPerPage={props.paginationPerPage}
        paginationTotalRows={props.paginationTotalRows}
        onChangePage={props.onChangePage}
        onChangeRowsPerPage={props.onChangeRowsPerPage}
        noDataComponent={
          props.noDataComponent ?? (
            <div className="py-3 text-center">
              <div>{props.noDataComponentText ?? t('noItemsFound')}</div>
            </div>
          )
        }
        subHeader={props.subHeader ?? false}
        subHeaderComponent={props.subHeaderComponent ?? <></>}
      />
    </div>
  )
}
