'use client'

import { useLazyQuery, useMutation, useQuery } from '@apollo/client'
import { trim } from 'lodash'
import { useLocale, useTranslations } from 'next-intl'
import { useCallback, useEffect, useMemo, useState } from 'react'
import { TableColumn } from 'react-data-table-component'
import { HiBolt, HiMiniTrash } from 'react-icons/hi2'
import { toast } from 'react-toastify'
import { Button, Tooltip } from 'reactstrap'
import Swal from 'sweetalert2'

import { Avatar } from '@/components/common/Avatar'
import Table, { SelectChangeState } from '@/components/common/Table'
import { APP_PAGINATION } from '@/environment'
import { PAGINATED_USERS, REMOVE_USER, REMOVE_USERS } from '@/graphql/users'
import { useAbility, useAuth } from '@/hooks'
import { Link, useRouter } from '@/navigation'
import { ActionEnum } from '@/types/action'
import {
  ModeEnum,
  OrderByEnum,
  PageWithChildrenProps,
  PaginatedInput,
} from '@/types/common'
import { AllSubjectsEnum } from '@/types/subject'
import { User } from '@/types/user'
import { DateTime } from '@/utils/date'

const defaultVariables: PaginatedInput = {
  page: 1,
  perPage: APP_PAGINATION,
  orderBy: { name: OrderByEnum.ASC },
  where: { deletedAt: null },
}

export const UsersList = ({ page, children }: PageWithChildrenProps) => {
  const t = useTranslations()
  const router = useRouter()
  const locale = useLocale()
  const ability = useAbility()
  const { timezone } = useAuth()

  const [name, setName] = useState('')
  const [rows, setRows] = useState<User[]>([])
  const [selectedRows, setSelectedRows] = useState<User[]>([])
  const [totalRows, setTotalRows] = useState(0)
  const [toggleCleared, setToggleCleared] = useState(false)
  const [tooltipOpen, setTooltipOpen] = useState(false)

  const [variables, setVariables] = useState<PaginatedInput>({
    ...defaultVariables,
    page,
  })

  const [removeUser] = useMutation(REMOVE_USER)
  const [removeUsers] = useMutation(REMOVE_USERS)
  const { data, loading, error } = useQuery(PAGINATED_USERS, {
    fetchPolicy: 'no-cache',
    variables,
  })
  const [handleUsers] = useLazyQuery(PAGINATED_USERS, {
    fetchPolicy: 'no-cache',
    variables,
  })

  const toggle = () => setTooltipOpen(!tooltipOpen)

  const handleSelectedRows = useCallback((state: SelectChangeState<User>) => {
    setSelectedRows(state.selectedRows)
  }, [])

  const handleSearch = useCallback(async () => {
    const where: any = {
      deletedAt: null,
      userKind: 'credential',
    }
    if (trim(name))
      where.AND = [
        {
          name: {
            contains: name,
            mode: ModeEnum.INSENSITIVE,
          },
        },
      ]
    setVariables({ ...variables, page: 1, where })
  }, [variables, name])

  const handleReset = useCallback(async () => {
    setName('')
    setVariables(defaultVariables)
    await handleUsers().then(({ data }) => {
      setRows(data?.paginatedUser.data || [])
      setTotalRows(data?.paginatedUser.meta.total || 0)
    })
  }, [handleUsers])

  const contextActions = useMemo(() => {
    const handleDelete = () => {
      Swal.fire({
        title: t('confirmation'),
        text: t('itemsDeleteConfirm'),
        icon: 'question',
        reverseButtons: true,
        showCancelButton: true,
        confirmButtonText: t('yes'),
        cancelButtonText: t('no'),
      }).then(async ({ isConfirmed }) => {
        if (isConfirmed) {
          await removeUsers({
            variables: { ids: selectedRows.map((row) => row.id) },
          })
            .then(async () => {
              await handleReset()
              toast.success(t('itemsRemoveSuccess'))
              setToggleCleared(!toggleCleared)
            })
            .catch((err) => toast.error(err.message))
        } else {
          setToggleCleared(!toggleCleared)
        }
      })
    }

    return (
      <Button
        onClick={handleDelete}
        size="sm"
        color="danger"
        disabled={loading}
      >
        <HiMiniTrash className="h-4 w-4" />
        {t('delete')}
      </Button>
    )
  }, [loading, t, removeUsers, selectedRows, handleReset, toggleCleared])

  const handleDelete = useCallback(
    async (id: string) => {
      Swal.fire({
        title: t('confirmation'),
        text: t('itemDeleteConfirm'),
        icon: 'question',
        reverseButtons: true,
        showCancelButton: true,
        confirmButtonText: t('yes'),
        cancelButtonText: t('no'),
      }).then(async ({ isConfirmed }) => {
        if (isConfirmed) {
          await removeUser({ variables: { id } })
            .then(async () => {
              await handleReset()
              toast.success(t('itemRemoveSuccess'))
            })
            .catch((err) => toast.error(err.message))
        }
      })
    },
    [handleReset, removeUser, t]
  )

  const columns: TableColumn<User>[] = [
    {
      name: t('name'),
      sortable: true,
      sortField: 'name',
      selector: (row) => row.name,
      cell: (row) => (
        <>
          <Avatar image={row.image} name={row.name} size={32} />
          {row.name}
        </>
      ),
    },
    {
      name: t('systemName'),
      sortable: true,
      sortField: 'systemName',
      selector: (row) => row.systemName,
      width: '120px',
    },
    {
      name: t('activated'),
      sortable: true,
      width: '100px',
      sortField: 'isActivated',
      cell: (row) => (
        <span
          className={`badge badge-light-${row.isActive ? 'success' : 'danger'}`}
        >
          {row.isActive ? t('activated') : t('inactivated')}
        </span>
      ),
    },
    {
      name: t('createdAt'),
      sortable: true,
      width: '200px',
      sortField: 'createdAt',
      cell: (row) => (
        <div className="flex flex-col">
          {new DateTime(row.createdAt)
            .timeZone(timezone)
            .locale(locale)
            .format()}
        </div>
      ),
    },
    {
      name: t('updatedAt'),
      sortable: true,
      width: '200px',
      sortField: 'updatedAt',
      cell: (row) => (
        <div className="flex flex-col">
          {new DateTime(row.updatedAt)
            .timeZone(timezone)
            .locale(locale)
            .format()}
        </div>
      ),
    },
    {
      name: <HiBolt className="h-4 w-4" />,
      width: '80px',
      center: true,
      cell: (row) => (
        <ul className="action">
          {ability.can(ActionEnum.Update, AllSubjectsEnum.User) && (
            <li className="edit">
              <Tooltip
                autohide
                flip
                target="update"
                isOpen={tooltipOpen}
                toggle={toggle}
              >
                {t('editName', { name: t('user') })}
              </Tooltip>
              <Link href={`/backend/users/edit/${row.id}`} id="update">
                <i className="icon-pencil-alt" />
              </Link>
            </li>
          )}
          {ability.can(ActionEnum.Delete, AllSubjectsEnum.User) && (
            <li className="delete">
              <Tooltip
                autohide
                flip
                target="update"
                isOpen={tooltipOpen}
                toggle={toggle}
              >
                {t('removeName', { name: t('user') })}
              </Tooltip>
              <Link href="#!" onClick={() => handleDelete(row.id)} id="delete">
                <i className="icon-trash" />
              </Link>
            </li>
          )}
        </ul>
      ),
    },
  ]

  useEffect(() => {
    if (error) toast.error(error.message)
    if (data) {
      setRows(data.paginatedUser.data)
      setTotalRows(data.paginatedUser.meta.total)
    }
  }, [error, data])

  return (
    <>
      <Table
        data={rows}
        columns={columns}
        defaultSortFieldId={1}
        progressPending={loading}
        contextActions={contextActions}
        selectableRows
        onSelectedRowsChange={handleSelectedRows}
        clearSelectedRows={toggleCleared}
        paginationPerPage={variables.perPage}
        highlightOnHover
        selectableRowDisabled={() =>
          !ability.can(ActionEnum.Delete, AllSubjectsEnum.User)
        }
        paginationTotalRows={totalRows}
        onChangePage={(page) => {
          setVariables({ ...variables, page })
        }}
        onChangeRowsPerPage={(currentRowsPerPage, currentPage) => {
          setVariables({
            ...variables,
            page: currentPage,
            perPage: currentRowsPerPage,
          })
        }}
        onSort={(selectedColumn, sortDirection) => {
          if (selectedColumn.sortField)
            setVariables({
              ...variables,
              orderBy: {
                [selectedColumn.sortField]: sortDirection.toLowerCase(),
              },
            })
        }}
      />
    </>
  )
}
