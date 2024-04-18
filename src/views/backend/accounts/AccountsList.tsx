'use client'

import { useLazyQuery, useMutation, useQuery } from '@apollo/client'
import { trim } from 'lodash'
import { useTranslations } from 'next-intl'
import { useCallback, useEffect, useMemo, useState } from 'react'
import { TableColumn } from 'react-data-table-component'
import { HiDotsVertical, HiRefresh } from 'react-icons/hi'
import { HiBolt, HiEye, HiPencilSquare, HiTrash } from 'react-icons/hi2'
import { toast } from 'react-toastify'
import { Tooltip } from 'react-tooltip'
import {
  Button,
  Card,
  CardBody,
  Col,
  Container,
  Dropdown,
  DropdownItem,
  DropdownMenu,
  DropdownToggle,
  FormGroup,
  Input,
  InputGroup,
  Label,
  Row,
} from 'reactstrap'
import Swal from 'sweetalert2'

import Can from '@/components/backend/Guards/Can'
import CanAny from '@/components/backend/Guards/CanAny'
import CommonCardHeading from '@/components/common/CommonCardHeading'
import Table, { SelectChangeState } from '@/components/common/Table'
import { APP_PAGINATION } from '@/environment'
import {
  DELETE_ACCOUNT,
  DELETE_ACCOUNTS,
  PAGINATED_ACCOUNTS,
  REMOVE_ACCOUNT,
  REMOVE_ACCOUNTS,
  RESTORE_ACCOUNT,
  RESTORE_ACCOUNTS,
} from '@/graphql/account'
import { useAbility } from '@/hooks'
import { Link } from '@/navigation'
import { Account } from '@/types/account'
import { ActionEnum } from '@/types/action'
import {
  FindManyInput,
  ModeEnum,
  OrderByEnum,
  PaginatedInput,
} from '@/types/common'
import { Subjects } from '@/types/subject'

import { ExpandedComponent } from './ExpandedComponent'

export const AccountsList = () => {
  const t = useTranslations()
  const ability = useAbility()
  const pageTitle = t('listName', { name: t('accounts') })
  const pageDescription = t('seeInformationAboutName', {
    gender: 'male',
    name: t('accounts').toLowerCase(),
  })
  const pageTrashTitle = t('nameTrash', { name: t('accounts') })
  const pageTrashDescription = t('seeInformationAboutDeletedName', {
    gender: 'male',
    name: t('accounts').toLowerCase(),
  })

  const defaultVariables: PaginatedInput = useMemo(() => {
    const variables: PaginatedInput = {
      page: 1,
      perPage: APP_PAGINATION,
      orderBy: { systemName: OrderByEnum.ASC },
      where: { deletedAt: null, AND: [], OR: [] },
    }

    if (variables.where?.AND?.length === 0) delete variables.where.AND
    if (variables.where?.OR?.length === 0) delete variables.where.OR

    return variables
  }, [])

  const [cardTitle, setCardTitle] = useState(pageTitle)
  const [cardDescription, setCardDescription] = useState(pageDescription)
  const [filterText, setFilterText] = useState('')
  const [rows, setRows] = useState<Account[]>([])
  const [selectedRows, setSelectedRows] = useState<Account[]>([])
  const [totalRows, setTotalRows] = useState(0)
  const [toggleCleared, setToggleCleared] = useState(false)
  const [displayError, setDisplayError] = useState(true)
  const [isTrash, setIsTrash] = useState(false)
  const [dropdownOpen, setDropdownOpen] = useState<any>({})
  const [variables, setVariables] = useState<PaginatedInput>(defaultVariables)

  const [removeAccount] = useMutation(REMOVE_ACCOUNT)
  const [removeAccounts] = useMutation(REMOVE_ACCOUNTS)
  const [deleteAccount] = useMutation(DELETE_ACCOUNT)
  const [deleteAccounts] = useMutation(DELETE_ACCOUNTS)
  const [restoreAccount] = useMutation(RESTORE_ACCOUNT)
  const [restoreAccounts] = useMutation(RESTORE_ACCOUNTS)
  const { data, loading, error } = useQuery(PAGINATED_ACCOUNTS, {
    fetchPolicy: 'no-cache',
    variables,
  })
  const [handleAccounts] = useLazyQuery(PAGINATED_ACCOUNTS, {
    fetchPolicy: 'no-cache',
    variables,
  })

  const toggleDropdown = useCallback(
    (id: string) => {
      setDropdownOpen({
        ...Object.keys(dropdownOpen).reduce(
          (acc, cur) => ({ ...acc, [cur]: false }),
          {}
        ),
        [id]: !dropdownOpen[id],
      })
    },
    [dropdownOpen]
  )

  const handleSelectedRows = useCallback(
    (state: SelectChangeState<Account>) => {
      setSelectedRows(state.selectedRows)
    },
    []
  )

  const handleSearch = useCallback(
    async (e?: React.FormEvent<HTMLFormElement> | null, trash?: boolean) => {
      const isTrashed = trash ?? isTrash

      if (typeof e !== 'undefined') {
        e?.preventDefault()
      }

      const where: FindManyInput = {
        deletedAt: null,
        AND: [],
        OR: [],
      }

      if (isTrashed) {
        delete where.deletedAt
        where.AND.push({ deletedAt: { not: null } })

        setCardTitle(pageTrashTitle)
        setCardDescription(pageTrashDescription)
      } else {
        setCardTitle(pageTitle)
        setCardDescription(pageDescription)
      }

      if (trim(filterText)) {
        where.OR.push({
          name: {
            contains: filterText,
            mode: ModeEnum.INSENSITIVE,
          },
        })
        where.OR.push({
          slug: {
            contains: filterText,
            mode: ModeEnum.INSENSITIVE,
          },
        })
      }

      if (where.AND?.length === 0) delete where.AND
      if (where.OR?.length === 0) delete where.OR

      setVariables({ ...variables, page: 1, where })
    },
    [
      isTrash,
      filterText,
      pageDescription,
      pageTitle,
      pageTrashDescription,
      pageTrashTitle,
      variables,
    ]
  )

  const handleReset = useCallback(async () => {
    setIsTrash(false)
    setFilterText('')
    setVariables(defaultVariables)
    setCardTitle(pageTitle)
    setCardDescription(pageDescription)
    await handleAccounts().then(({ data }) => {
      setRows(data?.paginatedAccount.data || [])
      setDropdownOpen(
        data?.paginatedAccount.data.reduce(
          (acc, cur) => ({ ...acc, [cur.id]: false }),
          {}
        )
      )
      setTotalRows(data?.paginatedAccount.meta.total || 0)
    })
  }, [defaultVariables, handleAccounts, pageDescription, pageTitle])

  const toggle = useCallback(async () => {
    const trash = !isTrash

    setIsTrash(trash)

    await handleSearch(null, trash)
  }, [handleSearch, isTrash])

  const contextActions = useMemo(() => {
    const handleRestore = () => {
      Swal.fire({
        title: t('confirmation'),
        text: t('itemsRestoreConfirm'),
        icon: 'question',
        showCancelButton: true,
        confirmButtonText: t('yes'),
        cancelButtonText: t('no'),
      }).then(async ({ isConfirmed }) => {
        if (isConfirmed) {
          await restoreAccounts({
            variables: { ids: selectedRows.map((row) => row.id) },
          })
            .then(async ({ data }) => {
              const res = data?.restoreManyAccount ?? false
              if (res) {
                await handleReset()
                toast.success(t('itemsRestoreSuccess'))
                setToggleCleared(!toggleCleared)
              } else {
                toast.error(t('itemsRestoreError'))
              }
            })
            .catch((err) => toast.error(err?.message ?? t('itemsRestoreError')))
        } else {
          setToggleCleared(!toggleCleared)
        }
      })
    }

    const handleDelete = () => {
      Swal.fire({
        title: t('confirmation'),
        text: isTrash ? t('itemsDeleteConfirm') : t('itemsRemoveConfirm'),
        icon: 'question',
        showCancelButton: true,
        confirmButtonText: t('yes'),
        cancelButtonText: t('no'),
      }).then(async ({ isConfirmed }) => {
        if (isConfirmed) {
          if (isTrash) {
            await deleteAccounts({
              variables: { ids: selectedRows.map((row) => row.id) },
            })
              .then(async ({ data }) => {
                const res = data?.deleteManyAccount ?? false
                if (res) {
                  await handleReset()
                  toast.success(t('itemsDeleteSuccess'))
                  setToggleCleared(!toggleCleared)
                } else {
                  toast.error(t('itemsDeleteError'))
                }
              })
              .catch((err) =>
                toast.error(err?.message ?? t('itemsDeleteError'))
              )
          } else {
            await removeAccounts({
              variables: { ids: selectedRows.map((row) => row.id) },
            })
              .then(async ({ data }) => {
                const res = data?.removeManyAccount ?? false
                if (res) {
                  await handleReset()
                  toast.success(t('itemsRemoveSuccess'))
                  setToggleCleared(!toggleCleared)
                } else {
                  toast.error(t('itemsRemoveError'))
                }
              })
              .catch((err) =>
                toast.error(err?.message ?? t('itemsRemoveError'))
              )
          }
        } else {
          setToggleCleared(!toggleCleared)
        }
      })
    }

    return (
      <>
        {isTrash && (
          <Button
            type="button"
            onClick={handleRestore}
            color="success"
            className="px-3 text-white me-2"
            disabled={loading}
          >
            <i className="fa fa-undo pe-2" aria-hidden="true"></i>
            {t('restore')}
          </Button>
        )}
        <Button
          type="button"
          onClick={handleDelete}
          color="danger"
          className="px-3 text-white"
          disabled={loading}
        >
          <i className="fa fa-trash pe-2" aria-hidden="true"></i>
          {t('remove')}
        </Button>
      </>
    )
  }, [
    loading,
    t,
    restoreAccounts,
    selectedRows,
    handleReset,
    toggleCleared,
    isTrash,
    deleteAccounts,
    removeAccounts,
  ])

  const handleDelete = useCallback(
    async (
      e: React.MouseEvent<HTMLAnchorElement> | React.MouseEvent<HTMLElement>,
      id: string
    ) => {
      e?.preventDefault()

      Swal.fire({
        title: t('confirmation'),
        text: isTrash ? t('itemDeleteConfirm') : t('itemRemoveConfirm'),
        icon: 'question',
        showCancelButton: true,
        confirmButtonText: t('yes'),
        cancelButtonText: t('no'),
      }).then(async ({ isConfirmed }) => {
        if (isConfirmed) {
          if (isTrash) {
            await deleteAccount({ variables: { id } })
              .then(async ({ data }) => {
                const res = data?.deleteAccount ?? false
                if (res) {
                  await handleReset()
                  toast.success(t('itemDeleteSuccess'))
                } else {
                  toast.error(t('itemDeleteError'))
                }
              })
              .catch((err) => toast.error(err?.message ?? t('itemDeleteError')))
          } else {
            await removeAccount({ variables: { id } })
              .then(async ({ data }) => {
                const res = data?.removeAccount ?? false
                if (res) {
                  await handleReset()
                  toast.success(t('itemRemoveSuccess'))
                } else {
                  toast.error(t('itemRemoveError'))
                }
              })
              .catch((err) => toast.error(err?.message ?? t('itemRemoveError')))
          }
        }
      })
    },
    [t, isTrash, deleteAccount, handleReset, removeAccount]
  )

  const handleRestore = useCallback(
    async (
      e: React.MouseEvent<HTMLAnchorElement> | React.MouseEvent<HTMLElement>,
      id: string
    ) => {
      e?.preventDefault()

      Swal.fire({
        title: t('confirmation'),
        text: t('itemRestoreConfirm'),
        icon: 'question',
        showCancelButton: true,
        confirmButtonText: t('yes'),
        cancelButtonText: t('no'),
      }).then(async ({ isConfirmed }) => {
        if (isConfirmed) {
          await restoreAccount({ variables: { id } })
            .then(async ({ data }) => {
              const res = data?.restoreAccount ?? false
              if (res) {
                await handleReset()
                toast.success(t('itemRestoreSuccess'))
              } else {
                toast.error(t('itemRestoreError'))
              }
            })
            .catch((err) => toast.error(err?.message ?? t('itemRestoreError')))
        }
      })
    },
    [handleReset, restoreAccount, t]
  )

  const columns: TableColumn<Account>[] = useMemo(
    () => [
      {
        name: t('tradingName'),
        sortable: true,
        sortField: 'tradingName',
        selector: (row) => row.tradingName,
      },
      {
        name: t('systemName'),
        sortable: true,
        sortField: 'systemName',
        selector: (row) => row.systemName,
      },
      {
        name: t('corporateNumber'),
        sortable: true,
        sortField: 'corporateNumber',
        width: '200px',
        selector: (row) => row.corporateNumber,
      },
      {
        name: <HiBolt className="h-4 w-4" />,
        center: true,
        width: '70px',
        cell: (row) => (
          <CanAny
            acls={[
              { action: ActionEnum.Read, subject: Subjects.Account },
              { action: ActionEnum.Update, subject: Subjects.Account },
              { action: ActionEnum.Delete, subject: Subjects.Account },
              { action: ActionEnum.Manage, subject: Subjects.Account },
            ]}
          >
            <Dropdown
              isOpen={dropdownOpen[row.id] ?? false}
              toggle={() => toggleDropdown(row.id)}
            >
              <DropdownToggle
                color="transparent"
                className="btn-dotted d-flex align-items-start justify-content-center"
                caret={false}
              >
                <HiDotsVertical />
              </DropdownToggle>
              <DropdownMenu flip>
                <DropdownItem
                  header
                  className="d-flex justify-content-center align-items-center"
                >
                  {t('actions')}
                </DropdownItem>
                <Can action={ActionEnum.Read} subject={Subjects.Account}>
                  <DropdownItem
                    className="d-flex justify-content-start align-items-center"
                    href={`/backend/accounts/view/${row.id}`}
                    tag={Link}
                  >
                    <HiEye className="me-2" />
                    {t('viewName', { name: t('role') })}
                  </DropdownItem>
                </Can>
                {!isTrash && (
                  <CanAny
                    acls={[
                      { action: ActionEnum.Update, subject: Subjects.Account },
                      { action: ActionEnum.Manage, subject: Subjects.Account },
                    ]}
                  >
                    <DropdownItem
                      className="d-flex justify-content-start align-items-center"
                      href={`/backend/accounts/edit/${row.id}`}
                      tag={Link}
                    >
                      <HiPencilSquare className="me-2" />
                      {t('editName', { name: t('role') })}
                    </DropdownItem>
                  </CanAny>
                )}
                {isTrash && (
                  <CanAny
                    acls={[
                      { action: ActionEnum.Delete, subject: Subjects.Account },
                      { action: ActionEnum.Manage, subject: Subjects.Account },
                    ]}
                  >
                    <DropdownItem
                      className="d-flex justify-content-start align-items-center"
                      href="#!"
                      onClick={(e) => handleRestore(e, row.id)}
                      tag={Link}
                    >
                      <HiRefresh className="me-2" />
                      {t('restoreName', { name: t('role') })}
                    </DropdownItem>
                  </CanAny>
                )}
                <CanAny
                  acls={[
                    { action: ActionEnum.Delete, subject: Subjects.Account },
                    { action: ActionEnum.Manage, subject: Subjects.Account },
                  ]}
                >
                  <DropdownItem
                    className="d-flex justify-content-start align-items-center"
                    href="#!"
                    onClick={(e) => handleDelete(e, row.id)}
                    tag={Link}
                  >
                    <HiTrash className="me-2" />
                    {t('deleteName', { name: t('role') })}
                  </DropdownItem>
                </CanAny>
              </DropdownMenu>
            </Dropdown>
          </CanAny>
        ),
      },
    ],
    [t, dropdownOpen, isTrash, toggleDropdown, handleRestore, handleDelete]
  )

  const subHeaderComponentMemo = useMemo(() => {
    return (
      <form onSubmit={handleSearch} className="dataTables_filter">
        <Container fluid className="px-0 gx-5">
          <Row>
            <Col lg={6} sm={12} className="text-lg-start">
              <CanAny
                acls={[
                  { action: ActionEnum.Manage, subject: Subjects.Account },
                  { action: ActionEnum.Delete, subject: Subjects.Account },
                ]}
              >
                {isTrash ? (
                  <Button
                    type="button"
                    color="light"
                    disabled={loading}
                    className="me-2 "
                    onClick={toggle}
                  >
                    <i className="fa fa-reply me-2" /> {t('back')}
                  </Button>
                ) : (
                  <Button
                    type="button"
                    color="light"
                    disabled={loading}
                    className="me-2 "
                    onClick={toggle}
                  >
                    <i className="fa fa-trash me-2" /> {t('recycleBin')}
                  </Button>
                )}
              </CanAny>
            </Col>
            <Col lg={6} sm={12}>
              <FormGroup row className="justify-content-lg-end">
                <Label for="search" lg={3}>
                  {t('search')}:
                </Label>
                <Col lg={6}>
                  <InputGroup>
                    <Input
                      id="search"
                      onChange={(e) => setFilterText(e.target.value)}
                      type="text"
                      disabled={loading}
                      value={filterText}
                    />
                    <Button
                      type="submit"
                      color="primary"
                      disabled={loading}
                      className="text-white"
                    >
                      <i className="fa fa-search" />
                    </Button>
                  </InputGroup>
                </Col>
              </FormGroup>
            </Col>
          </Row>
        </Container>
      </form>
    )
  }, [handleSearch, isTrash, loading, toggle, t, filterText])

  useEffect(() => {
    if (error && displayError) {
      toast.error(error.message)
      setDisplayError(false)
    }
    if (data) {
      setRows(data.paginatedAccount.data)
      setDropdownOpen(
        data.paginatedAccount.data.reduce(
          (acc, cur) => ({ ...acc, [cur.id]: false }),
          {}
        )
      )
      setTotalRows(data.paginatedAccount.meta.total)
    }
  }, [error, data, pageTitle, pageDescription, displayError])

  return (
    <>
      <Card>
        <CommonCardHeading smallHeading={cardTitle} span={cardDescription} />
        <CardBody>
          <Tooltip id="tooltip" />
          <div className="table-responsive d-flex">
            <Table
              data={rows}
              columns={columns}
              defaultSortFieldId={1}
              disabled={loading}
              progressPending={loading}
              contextActions={contextActions}
              selectableRows
              onSelectedRowsChange={handleSelectedRows}
              clearSelectedRows={toggleCleared}
              paginationPerPage={variables.perPage}
              highlightOnHover
              selectableRowDisabled={() =>
                !ability.can(ActionEnum.Delete, Subjects.Account)
              }
              pagination
              paginationServer
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
                      [selectedColumn.sortField]: sortDirection.toUpperCase(),
                    },
                  })
              }}
              className="display dataTable"
              subHeader
              subHeaderComponent={subHeaderComponentMemo}
              expandableRows
              expandableRowsComponent={ExpandedComponent}
              spinner={{ type: 'border' }}
              noDataComponentText={t('noDataNameText', {
                name: t('account').toLowerCase(),
                gender: 'male',
              })}
            />
          </div>
        </CardBody>
      </Card>
    </>
  )
}
