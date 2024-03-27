'use client'

import { useLazyQuery, useMutation } from '@apollo/client'
import { yupResolver } from '@hookform/resolvers/yup'
import { trim } from 'lodash'
import { useTranslations } from 'next-intl'
import { useCallback, useEffect, useState } from 'react'
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
import { FIND_MANY_ACCOUNTS } from '@/graphql/account'
import { FIND_MANY_CLAIMS } from '@/graphql/claims'
import { CREATE_ROLE, FIND_ROLE, UPDATE_ROLE } from '@/graphql/roles'
import { FIND_MANY_USERS } from '@/graphql/users'
import { useAppSelector } from '@/hooks'
import { useRouter } from '@/navigation'
import { Claim } from '@/types/claim'
import { OrderByEnum, ReactSelectType } from '@/types/common'
import { CreateRoleInput, Role, RoleInput, UpdateRoleInput } from '@/types/role'
import { User } from '@/types/user'
import { setFormValues, validateFormValue } from '@/utils/helpers'

type GroupsFormProps = {
  id?: string
  mode: 'create' | 'update' | 'view'
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

  const [getAccounts] = useLazyQuery(FIND_MANY_ACCOUNTS, {
    variables: {
      orderBy: { systemName: OrderByEnum.ASC },
    },
  })
  const [getClaims] = useLazyQuery(FIND_MANY_CLAIMS)
  const [getUsers] = useLazyQuery(FIND_MANY_USERS, {
    variables: {
      orderBy: { systemName: OrderByEnum.ASC },
    },
  })
  const [getRole] = useLazyQuery(FIND_ROLE, {
    variables: { id: `${id}` },
    fetchPolicy: 'no-cache',
  })
  const [createRole] = useMutation(CREATE_ROLE)
  const [updateRole] = useMutation(UPDATE_ROLE, { fetchPolicy: 'no-cache' })

  const t = useTranslations()
  const router = useRouter()
  const { user } = useAppSelector((state) => state.auth)

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

  const handleRole = useCallback(async () => {
    setDisabled(true)
    if (mode === 'update' || mode === 'view') {
      const [role, accounts, claims, users] = await Promise.all([
        getRole(),
        getAccounts(),
        getClaims(),
        getUsers(),
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
      if (accounts?.data?.findManyAccount)
        setAccounts(
          accounts?.data?.findManyAccount?.map(
            ({ id, systemName }) =>
              ({
                value: id,
                label: systemName,
              }) as ReactSelectType
          )
        )
      if (claims?.data?.findManyClaim) setClaims(claims?.data?.findManyClaim)
      if (users?.data?.findManyUser) setUsers(users?.data?.findManyUser)

      if (role?.error) toast.error(role?.error?.message)
      if (accounts?.error) toast.error(accounts?.error?.message)
      if (claims?.error) toast.error(claims?.error?.message)
      if (users?.error) toast.error(users?.error?.message)
    } else {
      const [accounts, claims, users] = await Promise.all([
        getAccounts(),
        getClaims(),
        getUsers(),
      ])

      if (accounts?.data?.findManyAccount)
        setAccounts(
          accounts?.data?.findManyAccount?.map(
            ({ id, systemName }) =>
              ({
                value: id,
                label: systemName,
              }) as ReactSelectType
          )
        )
      if (claims?.data?.findManyClaim) setClaims(claims?.data?.findManyClaim)
      if (users?.data?.findManyUser) setUsers(users?.data?.findManyUser)

      if (accounts?.error) toast.error(accounts?.error?.message)
      if (claims?.error) toast.error(claims?.error?.message)
      if (users?.error) toast.error(users?.error?.message)
    }
    setDisabled(false)
  }, [getClaims, getAccounts, getRole, getUsers, mode, setValue])

  const onSubmit = async (form: FormData) => {
    const isDeleteable = role?.isDeleteable ?? false

    const data: RoleInput = {
      accountId: form.accountId ?? null,
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

            router.push(`/backend/system/groups/edit/${data.createRole.id}`)
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
            <div className={`gx-3 pt-4 px-4 ${level === 1 ? '' : 'd-none'}`}>
              {user?.isSuperAdmin ? (
                <Row className="g-3">
                  <Col lg={12}>
                    <FormGroup row>
                      <Label for="accountId" sm={2} className="fw-bold px-4">
                        {t('account')}:
                      </Label>
                      <Col sm={10} className="px-4">
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
                      <Label for="accountId" sm={2} className="fw-bold px-4">
                        {t('account')}:
                      </Label>
                      <Col sm={10} className="px-4">
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
                                  (option) => option.value === user?.accountId
                                )?.label ?? t('none', { gender: 'male' })
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
                    <Label for="name" sm={4} className="fw-bold px-4 required">
                      {t('name')}:
                    </Label>
                    <Col sm={8} className="px-4">
                      <Controller
                        control={control}
                        name="name"
                        disabled={disabled}
                        rules={{ required: true }}
                        render={({ field: { name, onChange, ...rest } }) => (
                          <Input
                            id={name}
                            autoFocus
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
                    <Label for="slug" sm={4} className="fw-bold px-4 required">
                      {t('slug')}:
                    </Label>
                    <Col sm={8} className="px-4">
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
                    <Label for="isDefault" sm={4} className="fw-bold px-4">
                      {t('default')}:
                    </Label>
                    <Col sm={8} className="px-4">
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

            <Row className={`gx-3 pt-4 px-4 ${level === 2 ? '' : 'd-none'}`}>
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

            <Row className={`gx-3 pt-4 px-4 ${level === 3 ? '' : 'd-none'}`}>
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

            <div className={`gx-3 pt-4 px-4 ${level === 4 ? '' : 'd-none'}`}>
              <FinishForm />
            </div>
          </div>
        </CardBody>
        <CardFooter>
          <Row>
            <Col lg={6} sm={12}>
              <Button
                color="gray"
                className="px-4"
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
                    className="px-4"
                    onClick={handleBackButton}
                  >
                    <i className="fa fa-arrow-left me-2"></i>
                    {t('back')}
                  </Button>
                )}
                {level < 4 && (
                  <Button
                    type="button"
                    disabled={disabled}
                    color="primary"
                    className="px-4"
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
                    className="px-4"
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
