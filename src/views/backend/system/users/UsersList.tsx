'use client'

import { useLazyQuery, useMutation, useQuery } from '@apollo/client'
import { trim } from 'lodash'
import { useTranslations } from 'next-intl'
import { useCallback, useEffect, useMemo, useState } from 'react'
import { TableColumn } from 'react-data-table-component'
import { HiDotsVertical } from '@react-icons/all-files/hi/HiDotsVertical'
import { HiRefresh } from '@react-icons/all-files/hi/HiRefresh'
import { HiLightningBolt } from '@react-icons/all-files/hi/HiLightningBolt'
import { HiOutlineEye } from '@react-icons/all-files/hi/HiOutlineEye'
import { HiOutlineTrash } from '@react-icons/all-files/hi/HiOutlineTrash'
import { HiOutlinePencilAlt } from '@react-icons/all-files/hi/HiOutlinePencilAlt'
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
import Avatar from '@/components/common/Avatar'
import CommonCardHeading from '@/components/common/CommonCardHeading'
import Table, { SelectChangeState } from '@/components/common/Table'
import { APP_PAGINATION } from '@/environment'
import {
  DELETE_USER,
  DELETE_USERS,
  PAGINATED_USERS,
  REMOVE_USER,
  REMOVE_USERS,
  RESTORE_USER,
  RESTORE_USERS,
} from '@/graphql/users'
import { useAbility, useAppSelector } from '@/hooks'
import { Link } from '@/navigation'
import { ActionEnum } from '@/types/action'
import {
  FindManyInput,
  ModeEnum,
  OrderByEnum,
  PaginatedInput,
} from '@/types/common'
import { UserTypeEnum } from '@/types/enums'
import { Subjects } from '@/types/subject'
import { User } from '@/types/user'

import { ExpandedComponent } from './ExpandedComponent'

export const UsersList = () => {
  const t = useTranslations()
  const ability = useAbility()
  const pageTitle = t('listName', { name: t('users') })
  const pageDescription = t('seeInformationAboutName', {
    gender: 'male',
    name: t('users').toLowerCase(),
  })
  const pageTrashTitle = t('nameTrash', { name: t('users') })
  const pageTrashDescription = t('seeInformationAboutDeletedName', {
    gender: 'male',
    name: t('users').toLowerCase(),
  })

  const { jwt } = useAppSelector((state) => state.auth)

  const defaultVariables: PaginatedInput = useMemo(() => {
    const variables: PaginatedInput = {
      page: 1,
      perPage: APP_PAGINATION,
      orderBy: { name: OrderByEnum.ASC },
      where: {
        deletedAt: null,
        type: UserTypeEnum.CREDENTIAL,
        AND: [{ id: { not: { in: [jwt?.id || '123'] } } }],
        OR: [],
      },
    }

    // @ts-ignore
    if (!jwt?.sa && jwt?.accountId) variables.where['accountId'] = jwt.accountId

    if (variables.where?.AND?.length === 0) delete variables.where.AND
    if (variables.where?.OR?.length === 0) delete variables.where.OR

    return variables
  }, [jwt])

  const [cardTitle, setCardTitle] = useState(pageTitle)
  const [cardDescription, setCardDescription] = useState(pageDescription)
  const [filterText, setFilterText] = useState('')
  const [rows, setRows] = useState<User[]>([])
  const [selectedRows, setSelectedRows] = useState<User[]>([])
  const [totalRows, setTotalRows] = useState(0)
  const [toggleCleared, setToggleCleared] = useState(false)
  const [displayError, setDisplayError] = useState(true)
  const [isTrash, setIsTrash] = useState(false)
  const [dropdownOpen, setDropdownOpen] = useState<any>({})
  const [variables, setVariables] = useState<PaginatedInput>(defaultVariables)

  const [removeUser] = useMutation(REMOVE_USER)
  const [removeUsers] = useMutation(REMOVE_USERS)
  const [deleteUser] = useMutation(DELETE_USER)
  const [deleteUsers] = useMutation(DELETE_USERS)
  const [restoreUser] = useMutation(RESTORE_USER)
  const [restoreUsers] = useMutation(RESTORE_USERS)
  const { data, loading, error } = useQuery(PAGINATED_USERS, {
    fetchPolicy: 'no-cache',
    variables,
  })
  const [handleUsers] = useLazyQuery(PAGINATED_USERS, {
    fetchPolicy: 'no-cache',
    variables,
  })

  const toggleDropdown = useCallback(
    (id: number) => {
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

  const handleSelectedRows = useCallback((state: SelectChangeState<User>) => {
    setSelectedRows(state.selectedRows)
  }, [])

  const handleSearch = useCallback(
    async (e?: React.FormEvent<HTMLFormElement> | null, trash?: boolean) => {
      const isTrashed = trash ?? isTrash

      if (typeof e !== 'undefined') {
        e?.preventDefault()
      }

      const where: FindManyInput = {
        deletedAt: null,
        type: UserTypeEnum.CREDENTIAL,
        AND: [{ id: { not: { in: [jwt?.id || '123'] } } }],
        OR: [],
      }

      // @ts-ignore
      if (!jwt?.sa && jwt?.accountId) where['accountId'] = jwt.accountId

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
          systemName: {
            contains: filterText,
            mode: ModeEnum.INSENSITIVE,
          },
        })
        where.OR.push({
          email: {
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
      jwt,
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
    await handleUsers().then(({ data }) => {
      setRows(data?.paginatedUser.data || [])
      setDropdownOpen(
        data?.paginatedUser.data.reduce(
          (acc, cur) => ({ ...acc, [cur.id]: false }),
          {}
        )
      )
      setTotalRows(data?.paginatedUser.meta.total || 0)
    })
  }, [defaultVariables, handleUsers, pageDescription, pageTitle])

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
          await restoreUsers({
            variables: { ids: selectedRows.map((row) => row.id) },
          })
            .then(async ({ data }) => {
              const res = data?.restoreManyUser ?? false
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
            await deleteUsers({
              variables: { ids: selectedRows.map((row) => row.id) },
            })
              .then(async ({ data }) => {
                const res = data?.deleteManyUser ?? false
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
            await removeUsers({
              variables: { ids: selectedRows.map((row) => row.id) },
            })
              .then(async ({ data }) => {
                const res = data?.removeManyUser ?? false
                if (res) {
                  await handleReset()
                  toast.success(t('itemsRemoveSuccess'))
                  setToggleCleared(!toggleCleared)
                } else {
                  toast.error(t('itemsRemoveError'))
                }
              })
              .catch((err) => toast.error(err.message ?? t('itemsRemoveError')))
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
    restoreUsers,
    selectedRows,
    handleReset,
    toggleCleared,
    isTrash,
    deleteUsers,
    removeUsers,
  ])

  const handleDelete = useCallback(
    async (
      e: React.MouseEvent<HTMLAnchorElement> | React.MouseEvent<HTMLElement>,
      id: number
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
            await deleteUser({ variables: { id } })
              .then(async ({ data }) => {
                const res = data?.deleteUser ?? false
                if (res) {
                  await handleReset()
                  toast.success(t('itemDeleteSuccess'))
                } else {
                  toast.error(t('itemDeleteError'))
                }
              })
              .catch((err) => toast.error(err?.message ?? t('itemDeleteError')))
          } else {
            await removeUser({ variables: { id } })
              .then(async ({ data }) => {
                const res = data?.removeUser ?? false
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
    [t, isTrash, deleteUser, handleReset, removeUser]
  )

  const handleRestore = useCallback(
    async (
      e: React.MouseEvent<HTMLAnchorElement> | React.MouseEvent<HTMLElement>,
      id: number
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
          await restoreUser({ variables: { id } })
            .then(async ({ data }) => {
              const res = data?.restoreUser ?? false
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
    [handleReset, restoreUser, t]
  )

  const columns: TableColumn<User>[] = useMemo(
    () => [
      {
        name: t('name'),
        sortable: true,
        sortField: 'name',
        selector: (row) => row.name,
        cell: (row) => (
          <>
            <Avatar
              image={row.image}
              name={row.name}
              size={32}
              className="img-fluid table-avatar me-2"
              rounded
            />
            {row.name}
          </>
        ),
      },
      {
        name: t('systemName'),
        sortable: true,
        sortField: 'systemName',
        width: '200px',
        selector: (row) => row.systemName,
      },
      {
        name: t('email'),
        sortable: true,
        sortField: 'email',
        selector: (row) => row.email,
      },
      {
        name: t('active'),
        sortable: true,
        width: '90px',
        sortField: 'isActive',
        cell: (row) => (
          <span
            className={`badge badge-light-${row.isActive ? 'success' : 'danger'}`}
          >
            {row.isActive ? t('yes') : t('no')}
          </span>
        ),
      },
      {
        name: <HiLightningBolt className="h-4 w-4" />,
        center: true,
        width: '70px',
        cell: (row) => (
          <CanAny
            acls={[
              { action: ActionEnum.Read, subject: Subjects.User },
              { action: ActionEnum.Update, subject: Subjects.User },
              { action: ActionEnum.Delete, subject: Subjects.User },
              { action: ActionEnum.Manage, subject: Subjects.User },
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
                <Can action={ActionEnum.Read} subject={Subjects.User}>
                  <DropdownItem
                    className="d-flex justify-content-start align-items-center"
                    href={`/backend/system/users/view/${row.id}`}
                    tag={Link}
                  >
                    <HiOutlineEye className="me-2" />
                    {t('viewName', { name: t('user') })}
                  </DropdownItem>
                </Can>
                {!isTrash && (
                  <CanAny
                    acls={[
                      { action: ActionEnum.Update, subject: Subjects.User },
                      { action: ActionEnum.Manage, subject: Subjects.User },
                    ]}
                  >
                    <DropdownItem
                      className="d-flex justify-content-start align-items-center"
                      href={`/backend/system/users/edit/${row.id}`}
                      tag={Link}
                    >
                      <HiOutlinePencilAlt className="me-2" />
                      {t('editName', { name: t('user') })}
                    </DropdownItem>
                  </CanAny>
                )}
                {isTrash && (
                  <CanAny
                    acls={[
                      { action: ActionEnum.Delete, subject: Subjects.User },
                      { action: ActionEnum.Manage, subject: Subjects.User },
                    ]}
                  >
                    <DropdownItem
                      className="d-flex justify-content-start align-items-center"
                      href="#!"
                      onClick={(e) => handleRestore(e, row.id)}
                      tag={Link}
                    >
                      <HiRefresh className="me-2" />
                      {t('restoreName', { name: t('user') })}
                    </DropdownItem>
                  </CanAny>
                )}
                <CanAny
                  acls={[
                    { action: ActionEnum.Delete, subject: Subjects.User },
                    { action: ActionEnum.Manage, subject: Subjects.User },
                  ]}
                >
                  <DropdownItem
                    className="d-flex justify-content-start align-items-center"
                    href="#!"
                    onClick={(e) => handleDelete(e, row.id)}
                    tag={Link}
                  >
                    <HiOutlineTrash className="me-2" />
                    {t('deleteName', { name: t('user') })}
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
                  { action: ActionEnum.Manage, subject: Subjects.User },
                  { action: ActionEnum.Delete, subject: Subjects.User },
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
      setRows(data.paginatedUser.data)
      setDropdownOpen(
        data.paginatedUser.data.reduce(
          (acc, cur) => ({ ...acc, [cur.id]: false }),
          {}
        )
      )
      setTotalRows(data.paginatedUser.meta.total)
    }
  }, [error, data, pageTitle, pageDescription, displayError])

  return (
    <>
      <Card>
        <CommonCardHeading smallHeading={cardTitle} span={cardDescription} />
        <CardBody>
          <Tooltip id="tooltip" />
          <div className="table-responsive">
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
                !ability.can(ActionEnum.Delete, Subjects.User)
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
                name: t('user').toLowerCase(),
                gender: 'male',
              })}
            />
          </div>
        </CardBody>
      </Card>
    </>
  )
}
