'use client'

import { useTranslations } from 'next-intl'
import { useEffect, useState } from 'react'
import DataTable, { createTheme, TableProps } from 'react-data-table-component'
import { FaSortDown } from 'react-icons/fa'
import { HiMinusCircle, HiPlusCircle } from 'react-icons/hi2'

import { APP_PAGINATION_ROWS, APP_PAGINATION_SHOW_ALL } from '@/environment'
import { useAppSelector } from '@/hooks'

import { SpinnerLine } from './SpinnerLine'

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

export default function Table<T>(
  props: TableProps<T> & { selectAllRowsItem?: boolean }
) {
  const t = useTranslations()
  const [dataTableTheme, setDataTableTheme] = useState('tm-light')
  const { theme } = useAppSelector((state) => state.theme)

  useEffect(() => {
    setDataTableTheme(theme === 'light' ? 'tm-light' : 'tm-dark')
  }, [theme])

  return (
    <div className="table-responsive">
      <div className="dataTables_wrapper">
        <DataTable
          title={props.title || <></>}
          columns={props.columns}
          data={props.data}
          theme={dataTableTheme}
          responsive
          striped={false}
          progressPending={props.progressPending}
          progressComponent={
            <div className="mb-3">
              <SpinnerLine color="default" className="py-3" />
            </div>
          }
          paginationServerOptions={props.paginationServerOptions}
          paginationResetDefaultPage={props.paginationResetDefaultPage}
          paginationDefaultPage={props.paginationDefaultPage || 1}
          defaultSortAsc={props.defaultSortAsc || true}
          defaultSortFieldId={props.defaultSortFieldId}
          fixedHeader={props.fixedHeader}
          selectableRows={props.selectableRows}
          contextActions={props.contextActions}
          contextMessage={{
            singular: t('record'),
            plural: t('records'),
            message: `${t('selected')}.`,
          }}
          persistTableHead={props.persistTableHead || false}
          fixedHeaderScrollHeight={props.fixedHeaderScrollHeight}
          highlightOnHover={props.highlightOnHover}
          pointerOnHover={props.pointerOnHover}
          onSelectedRowsChange={props.onSelectedRowsChange}
          clearSelectedRows={props.clearSelectedRows}
          sortIcon={<FaSortDown className="ms-1" />}
          expandableRows={props.expandableRows}
          expandableRowsComponent={props.expandableRowsComponent}
          expandableIcon={{
            expanded: <HiMinusCircle className="h-5 w-5" />,
            collapsed: <HiPlusCircle className="h-5 w-5" />,
          }}
          onSort={props.onSort}
          pagination
          paginationComponentOptions={{
            rowsPerPageText: t('rowsPerPageText'),
            rangeSeparatorText: t('of'),
            selectAllRowsItem:
              props.selectAllRowsItem || APP_PAGINATION_SHOW_ALL,
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
          paginationServer
          paginationPerPage={props.paginationPerPage}
          paginationTotalRows={props.paginationTotalRows}
          onChangePage={props.onChangePage}
          onChangeRowsPerPage={props.onChangeRowsPerPage}
          noDataComponent={
            <div className="py-5 text-center">
              <div>{t('noItemsFound')}</div>
            </div>
          }
        />
      </div>
    </div>
  )
}
