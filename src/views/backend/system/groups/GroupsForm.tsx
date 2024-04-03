'use client'

import { useLazyQuery, useMutation } from '@apollo/client'
import { yupResolver } from '@hookform/resolvers/yup'
import { trim } from 'lodash'
import { useTranslations } from 'next-intl'
import { Fragment, useCallback, useEffect, useState } from 'react'
import { Controller, useForm } from 'react-hook-form'
import Select from 'react-select'
import { toast } from 'react-toastify'
import {
  Button,
  CardBody,
  CardFooter,
  Col,
  FormFeedback,
  FormGroup,
  Input,
  Label,
  Row,
} from 'reactstrap'
import slugify from 'slugify'
import * as yup from 'yup'

import FinishForm from '@/components/common/NumberingWizard/FinishForm'
import StepperHorizontal from '@/components/common/NumberingWizard/StepperHorizontal'
import { SLUG_REGEX } from '@/configs'
import { api } from '@/configs/axios'
import { CREATE_ROLE, FIND_ROLE, UPDATE_ROLE } from '@/graphql/roles'
import { useAuth } from '@/hooks'
import { useRouter } from '@/navigation'
import { Account } from '@/types/account'
import { Claim } from '@/types/claim'
import {
  OrderByEnum,
  ReactSelectType,
  WhereAndOrderInput,
} from '@/types/common'
import { UserTypeEnum } from '@/types/enums'
import { CreateRoleInput, Role, RoleInput, UpdateRoleInput } from '@/types/role'
import { ProtectedSubjectsEnum } from '@/types/subject'
import { User } from '@/types/user'
import { setFormValues, validateFormValue } from '@/utils/helpers'

type GroupsFormProps = {
  id?: string
  mode: 'create' | 'update'
}

type FormData = {
  accountId?: string | null
  name: string
  slug: string
  isDefault?: boolean
  isDeleteable?: boolean
  claims?: string[]
  users?: string[]
}

const defaultValues: FormData = {
  accountId: null,
  name: '',
  slug: '',
  isDefault: false,
  isDeleteable: true,
  claims: [],
  users: [],
}

export const GroupsForm = ({ id, mode }: GroupsFormProps) => {
  const [level, setLevel] = useState(1)
  const [disabled, setDisabled] = useState(false)
  const [role, setRole] = useState<Role>()
  const [accounts, setAccounts] = useState<ReactSelectType[]>([])
  const [claims, setClaims] = useState<Claim[]>([])
  const [users, setUsers] = useState<User[]>([])

  const { jwt } = useAuth()

  const [getRole] = useLazyQuery(FIND_ROLE, {
    variables: { id: `${id}` },
    fetchPolicy: 'no-cache',
  })
  const [createRole] = useMutation(CREATE_ROLE)
  const [updateRole] = useMutation(UPDATE_ROLE, { fetchPolicy: 'no-cache' })

  const t = useTranslations()
  const router = useRouter()

  const schema = yup.object().shape({
    name: yup
      .string()
      .trim()
      .required(t('propertyRequired', { property: t('name') })),
    slug: yup
      .string()
      .trim()
      .required(t('propertyRequired', { property: t('slug') }))
      .matches(
        new RegExp(SLUG_REGEX),
        t('propertySlug', { property: t('slug') })
      ),
    isDefault: yup.boolean().optional(),
    isDeleteable: yup.boolean().optional(),
    accounts: yup.array().optional(),
    claims: yup.array().optional(),
    users: yup.array().optional(),
  })

  const {
    control,
    setValue,
    getValues,
    handleSubmit,
    formState: { errors },
  } = useForm({
    defaultValues,
    mode: 'onBlur',
    resolver: yupResolver(schema),
  })

  const getAccounts = useCallback(
    async (data?: any) =>
      await api
        .post<Account[]>('/backend/accounts', data)
        .then(({ data }) =>
          setAccounts(
            data?.map(
              ({ id, systemName }) =>
                ({
                  value: id,
                  label: systemName,
                }) as unknown as ReactSelectType
            )
          )
        )
        .catch((error) => toast.error(error?.response?.data?.message)),
    []
  )

  const getClaims = useCallback(
    async () =>
      await api
        .get<Claim[]>('/backend/claims')
        .then(({ data }) => setClaims(data))
        .catch((error) => toast.error(error?.response?.data?.message)),
    []
  )

  const getUsers = useCallback(
    async (data?: any) =>
      await api
        .post<User[]>('/backend/users', data)
        .then(({ data }) => setUsers(data))
        .catch((error) => toast.error(error?.response?.data?.message)),
    []
  )

  const handleRole = useCallback(async () => {
    setDisabled(true)

    let accountsVariables: WhereAndOrderInput = {
      where: {
        deletedAt: null,
      },
      orderBy: { systemName: OrderByEnum.ASC },
    }
    let usersVariables: WhereAndOrderInput = {
      where: {
        AND: [{ id: { not: { in: [jwt?.id || '123'] } } }],
        deletedAt: null,
        type: UserTypeEnum.CREDENTIAL,
      },
      orderBy: { systemName: OrderByEnum.ASC },
    }

    if (!jwt?.sa && jwt?.accountId) {
      accountsVariables = {
        where: {
          id: jwt.accountId,
          deletedAt: null,
        },
        orderBy: { systemName: OrderByEnum.ASC },
      }

      usersVariables = {
        where: {
          AND: [{ id: { not: { in: [jwt.id] } } }],
          accountId: jwt.accountId,
          type: UserTypeEnum.CREDENTIAL,
          deletedAt: null,
        },
        orderBy: { systemName: OrderByEnum.ASC },
      }
    }

    if (mode === 'update') {
      const [role] = await Promise.all([
        getRole(),
        getAccounts(accountsVariables),
        getClaims(),
        getUsers(usersVariables),
      ])

      if (role?.data?.findByIdRole) {
        const { findByIdRole } = role.data
        const { users, ...rest } = findByIdRole

        setRole(findByIdRole)
        setFormValues(setValue, {
          ...rest,
          users: users?.map((user) => user.id),
        })
      }

      if (role?.error) toast.error(role?.error?.message)
    } else {
      await Promise.all([
        getAccounts(accountsVariables),
        getClaims(),
        getUsers(usersVariables),
      ])
    }
    setDisabled(false)
  }, [getClaims, getAccounts, getRole, getUsers, mode, setValue, jwt])

  const onSubmit = async (form: FormData) => {
    const isDeleteable = role?.isDeleteable ?? false

    const data: RoleInput = {
      accountId: jwt?.accountId ?? null,
      name: form.name,
      slug: slugify(form.slug).toLowerCase(),
      isDefault: form.isDefault,
      claims: form.claims,
      users: form.users,
    }

    if (role && !isDeleteable && mode === 'update') {
      data.name = role.name
      data.slug = role.slug
    }

    if (mode === 'create') {
      const variables: CreateRoleInput = {
        data,
      }
      setDisabled(true)

      await createRole({ variables })
        .then(({ data }) => {
          if (data?.createRole) {
            toast.success(t('nameCreatedSuccess', { name: t('role') }))

            router.push(`/backend/system/groups`)
          } else {
            toast.error(
              t('nameCreatedError', { name: t('role').toLowerCase() })
            )
          }
        })
        .catch((error) =>
          toast.error(
            error?.message ??
              t('nameCreatedError', { name: t('role').toLowerCase() })
          )
        )
    } else if (mode === 'update') {
      const variables: UpdateRoleInput = {
        data: {
          id: `${id}`,
          ...data,
        },
      }

      setDisabled(true)
      await updateRole({ variables })
        .then(({ data }) => {
          if (data?.updateRole) {
            toast.success(t('nameUpdatedSuccess', { name: t('role') }))
            const { updateRole } = data
            const { users, ...rest } = updateRole
            setFormValues(setValue, {
              ...rest,
              users: users?.map((user) => user.id),
            })
            router.push(`/backend/system/groups`)
          } else {
            toast.error(
              t('nameUpdatedError', { name: t('role').toLowerCase() })
            )
          }
        })
        .catch((error) =>
          toast.error(
            error?.message ??
              t('nameUpdatedError', { name: t('role').toLowerCase() })
          )
        )
    }

    setDisabled(false)
  }

  const handleBackButton = () => {
    if (level === 2) {
      setLevel(level - 1)
    }
    if (level === 3) {
      setLevel(level - 1)
    }
    if (level === 4) {
      setLevel(level - 1)
    }
  }

  const handleNextButton = () => {
    if (
      validateFormValue(getValues('name'), errors.name) &&
      validateFormValue(getValues('slug'), errors.slug) &&
      level === 1
    ) {
      setLevel(2)
    } else if (!Boolean(errors.name) && !Boolean(errors.slug) && level === 1) {
      toast.error(t('typeFieldsRequired'))
    }

    if (level === 2) {
      setLevel(3)
    } else if (level === 3) {
      setLevel(4)
    } else {
      const error = Object.values(errors).shift()
      error?.message && toast.error(error.message)
    }
  }

  useEffect(() => {
    handleRole()
  }, [handleRole])

  return (
    <>
      <form noValidate autoComplete="on" onSubmit={handleSubmit(onSubmit)}>
        <CardBody className="basic-wizard important-validation">
          <StepperHorizontal
            disabled={disabled}
            mode={mode}
            level={level}
            setLevel={setLevel}
            steps={[
              t('dataName', { name: t('role'), gender: 'male' }),
              t('permissions'),
              t('users'),
            ]}
          />
          <div className="step-content">
            <div className={`pt-3 ${level === 1 ? '' : 'd-none'}`}>
              {jwt?.sa ? (
                <Row className="g-3">
                  <Col lg={12}>
                    <FormGroup row>
                      <Label for="accountId" lg={2} className="fw-bold px-3">
                        {t('account')}:
                      </Label>
                      <Col lg={10} className="px-3">
                        <Controller
                          name="accountId"
                          control={control}
                          disabled={disabled}
                          rules={{ required: true }}
                          render={({
                            field: { name, onChange, value, disabled, ref },
                          }) => (
                            <div
                              className={`select2-input ${errors.accountId && errors.accountId.message ? 'has-error' : ''}`}
                            >
                              <Select
                                ref={ref}
                                name={name}
                                placeholder={t('selectOne')}
                                className="react-select-container"
                                classNamePrefix="react-select"
                                isDisabled={disabled ?? false}
                                onChange={(newValue) =>
                                  onChange(newValue?.value ?? '')
                                }
                                value={accounts.find(
                                  (option) => option.value === value
                                )}
                                options={
                                  [{ label: t('selectOne'), value: '' }].concat(
                                    accounts as any
                                  ) as any
                                }
                                noOptionsMessage={({ inputValue }) =>
                                  !trim(inputValue as string)
                                    ? ''
                                    : t('noResultsFound')
                                }
                              />
                              <input
                                type="hidden"
                                className={
                                  errors.accountId && errors.accountId.message
                                    ? 'is-invalid'
                                    : ''
                                }
                                value={value ?? ''}
                              />
                              <FormFeedback>
                                {errors.accountId && errors.accountId.message}
                              </FormFeedback>
                            </div>
                          )}
                        />
                      </Col>
                    </FormGroup>
                  </Col>
                </Row>
              ) : (
                <Row className="gx-3">
                  <Col lg={12}>
                    <FormGroup row>
                      <Label for="accountId" lg={2} className="fw-bold px-3">
                        {t('account')}:
                      </Label>
                      <Col lg={10} className="px-3">
                        <Controller
                          control={control}
                          name="accountId"
                          disabled={disabled}
                          rules={{ required: true }}
                          render={({ field: { name, ...rest } }) => (
                            <Input
                              id={name}
                              readOnly
                              invalid={Boolean(errors.accountId)}
                              {...rest}
                              value={
                                accounts.find(
                                  ({ value }) =>
                                    value === role?.accountId ||
                                    value === jwt?.accountId
                                )?.label ?? t('none', { gender: 'female' })
                              }
                            />
                          )}
                        />
                        {errors.accountId && (
                          <FormFeedback>
                            {errors.accountId.message}
                          </FormFeedback>
                        )}
                      </Col>
                    </FormGroup>
                  </Col>
                </Row>
              )}
              <Row className="g-3">
                <Col lg={6} sm={12}>
                  <FormGroup row>
                    <Label for="name" lg={4} className="fw-bold px-3 required">
                      {t('name')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <Controller
                        control={control}
                        name="name"
                        disabled={disabled}
                        rules={{ required: true }}
                        render={({ field: { name, onChange, ...rest } }) => (
                          <Input
                            id={name}
                            placeholder={t('typeName', {
                              gender: 'male',
                              name: t('name').toLowerCase(),
                            })}
                            readOnly={!getValues('isDeleteable')}
                            invalid={Boolean(errors.name)}
                            onChange={(e) => {
                              onChange(e.target.value)
                              setValue(
                                'slug',

                                slugify(e.target.value).toLowerCase()
                              )
                            }}
                            {...rest}
                          />
                        )}
                      />
                      {errors.name && (
                        <FormFeedback>{errors.name.message}</FormFeedback>
                      )}
                    </Col>
                  </FormGroup>
                </Col>
                <Col lg={6} sm={12}>
                  <FormGroup row>
                    <Label for="slug" lg={4} className="fw-bold px-3 required">
                      {t('slug')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <Controller
                        control={control}
                        name="slug"
                        disabled={disabled}
                        rules={{ required: true }}
                        render={({ field: { name, onChange, ...rest } }) => (
                          <Input
                            id={name}
                            autoComplete="off"
                            placeholder={t('typeName', {
                              gender: 'male',
                              name: t('slug').toLowerCase(),
                            })}
                            readOnly={!getValues('isDeleteable')}
                            invalid={Boolean(errors.slug)}
                            onChange={(e) =>
                              onChange(e.target.value.toLowerCase())
                            }
                            {...rest}
                          />
                        )}
                      />
                      {errors.slug && (
                        <FormFeedback>{errors.slug.message}</FormFeedback>
                      )}
                    </Col>
                  </FormGroup>
                </Col>
              </Row>
              <Row className="g-3">
                <Col lg={6} sm={12}>
                  <FormGroup row>
                    <Label for="isDefault" lg={4} className="fw-bold px-3">
                      {t('default')}:
                    </Label>
                    <Col lg={8} className="px-3">
                      <Controller
                        name="isDefault"
                        control={control}
                        disabled={disabled}
                        render={({ field: { name, value, ...rest } }) => (
                          <Input
                            id={name}
                            type="checkbox"
                            value="true"
                            checked={value}
                            {...rest}
                          />
                        )}
                      />
                    </Col>
                  </FormGroup>
                </Col>
              </Row>
            </div>

            <Row className={`pt-3 ${level === 2 ? '' : 'd-none'}`}>
              <Col>
                <FormGroup row>
                  <Controller
                    name="claims"
                    control={control}
                    disabled={disabled}
                    render={({ field: { onChange, value } }) => (
                      <>
                        {claims?.map(({ action, subject }) => {
                          const key = `${subject}:${action}`
                          const label = `${t(action)} ${t(subject)}`

                          if (!jwt?.sa && !jwt?.roles.includes('admin')) {
                            if (!jwt?.claims?.includes(key))
                              return <Fragment key={key}></Fragment>
                            if (
                              Object.keys(ProtectedSubjectsEnum).includes(
                                subject
                              )
                            )
                              return <Fragment key={key}></Fragment>
                          } else if (!jwt?.sa && jwt?.roles.includes('admin')) {
                            if (
                              Object.keys(ProtectedSubjectsEnum).includes(
                                subject
                              )
                            )
                              return <Fragment key={key}></Fragment>
                          }

                          return (
                            <Label key={key} for={key} lg={4} sm={6}>
                              <Input
                                id={key}
                                type="checkbox"
                                className="me-2"
                                value={key}
                                checked={value && value?.includes(key)}
                                onChange={(e) => {
                                  if (e.target.checked) {
                                    value && onChange([...value, key])
                                  } else {
                                    value &&
                                      onChange(
                                        value.filter((val) => val !== key)
                                      )
                                  }
                                }}
                              />
                              {label}
                            </Label>
                          )
                        })}
                      </>
                    )}
                  />
                </FormGroup>
              </Col>
            </Row>

            <Row className={`pt-3 ${level === 3 ? '' : 'd-none'}`}>
              <Col>
                <FormGroup row>
                  <Controller
                    name="users"
                    control={control}
                    disabled={disabled}
                    render={({ field: { onChange, value } }) => (
                      <>
                        {users?.map(({ id, systemName, name }) => {
                          return (
                            <Label key={id} htmlFor={id} lg={4} sm={6}>
                              <Input
                                id={id}
                                type="checkbox"
                                className="me-2"
                                value={id}
                                checked={value && value?.includes(id)}
                                onChange={(e) => {
                                  if (e.target.checked) {
                                    value && onChange([...value, id])
                                  } else {
                                    value &&
                                      onChange(
                                        value.filter((val) => val !== id)
                                      )
                                  }
                                }}
                              />
                              {systemName} ({name})
                            </Label>
                          )
                        })}
                      </>
                    )}
                  />
                </FormGroup>
              </Col>
            </Row>

            <div className={`pt-3 ${level === 4 ? '' : 'd-none'}`}>
              <FinishForm />
            </div>
          </div>
        </CardBody>
        <CardFooter>
          <Row>
            <Col lg={6} sm={12}>
              <Button
                color="gray"
                className="px-3"
                type="button"
                disabled={disabled}
                onClick={() => router.push('/backend/system/groups')}
              >
                <i className="fa fa-times-circle me-2"></i>
                {t('cancel')}
              </Button>
            </Col>
            <Col lg={6} sm={12} className="justify-content-lg-end d-flex">
              <div className="wizard-footer d-flex gap-2 justify-content-end">
                {level > 1 && (
                  <Button
                    type="button"
                    color="secondary"
                    className="px-3"
                    disabled={disabled}
                    onClick={handleBackButton}
                  >
                    <i className="fa fa-arrow-left me-2"></i>
                    {t('back')}
                  </Button>
                )}
                {level < 4 && (
                  <Button
                    type="button"
                    color="primary"
                    className="px-3"
                    disabled={disabled}
                    onClick={handleNextButton}
                  >
                    <i className="fa fa-arrow-right me-2"></i>
                    {t('next')}
                  </Button>
                )}
                {level === 4 && (
                  <Button
                    type="submit"
                    disabled={disabled}
                    color="primary"
                    className="px-3"
                  >
                    <i className="fa fa-save me-2"></i>
                    {t('save')}
                  </Button>
                )}
              </div>
            </Col>
          </Row>
        </CardFooter>
      </form>
    </>
  )
}
