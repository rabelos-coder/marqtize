'use client'

import { yupResolver } from '@hookform/resolvers/yup'
import axios from 'axios'
import { useLocale, useTranslations } from 'next-intl'
import { useReCaptcha } from 'next-recaptcha-v3'
import { useState } from 'react'
import { Controller, useForm } from 'react-hook-form'
import { toast } from 'react-toastify'
import {
  Button,
  Col,
  FormFeedback,
  FormGroup,
  Input,
  Label,
  Row,
} from 'reactstrap'
import * as yup from 'yup'

import { CommonLogo } from '@/components/common/CommonLogo'
import { EMAIL_REGEX, PASSWORD_STRENGTH_REGEX } from '@/configs'
import { IS_DEVELOPMENT } from '@/environment'
import { useAppSelector } from '@/hooks'
import { Link, useRouter } from '@/navigation'
import { AuthFormProps } from '@/types/common'

type FormData = {
  name: string
  systemName: string
  email: string
  password: string
  passwordConfirmation: string
}

const defaultValues = IS_DEVELOPMENT
  ? {
      name: 'Amanda Smith',
      systemName: 'Amanda',
      email: 'amandasmith@me.com',
      password: 'abClK1@X',
      passwordConfirmation: 'abClK1@X',
    }
  : {
      name: '',
      systemName: '',
      email: '',
      password: '',
      passwordConfirmation: '',
    }

export const RegisterForm = ({ alignLogo }: AuthFormProps) => {
  const [disabled, setDisabled] = useState(false)
  const [showPassword, setShowPassword] = useState(false)
  const [showPasswordConfirmation, setShowPasswordConfirmation] =
    useState(false)

  const t = useTranslations()
  const router = useRouter()
  const locale = useLocale()
  const { executeRecaptcha } = useReCaptcha()
  const { account } = useAppSelector((state) => state.account)

  const schema = yup.object().shape({
    name: yup.string().required(t('propertyRequired', { property: t('name') })),
    systemName: yup
      .string()
      .required(t('propertyRequired', { property: t('systemName') })),
    email: yup
      .string()
      .email(t('propertyEmail', { property: t('email') }))
      .required(t('propertyRequired', { property: t('email') }))
      .matches(
        new RegExp(EMAIL_REGEX),
        t('propertyEmail', { property: t('email') })
      ),
    password: yup
      .string()
      .trim()
      .required(t('propertyRequired', { property: t('password') }))
      .matches(
        new RegExp(PASSWORD_STRENGTH_REGEX),
        t('propertyStrength', { property: t('password') })
      ),
    passwordConfirmation: yup
      .string()
      .trim()
      .required(t('propertyRequired', { property: t('passwordConfirmation') }))
      .oneOf(
        [yup.ref('password')],
        t('propertyMatch', {
          property: t('passwordConfirmation'),
          match: t('password'),
        })
      ),
  })

  const {
    control,
    handleSubmit,
    formState: { errors },
  } = useForm({
    defaultValues,
    mode: 'onBlur',
    resolver: yupResolver(schema),
  })

  const onSubmit = async (form: FormData) => {
    setDisabled(true)

    const data = {
      accountId: account?.id ?? null,
      name: form.name,
      systemName: form.systemName,
      email: form.email,
      password: form.password,
    }

    const recaptcha = await executeRecaptcha('form_submit')

    if (!recaptcha) {
      toast.error(t('reCaptchaError'))

      return
    }

    await axios
      .post(`/api/auth/register`, data, {
        headers: { recaptcha, locale },
      })
      .then(({ data }) => {
        if (data) {
          toast.success(t('registerSuccess'))
          router.push('/auth/login')
        } else {
          toast.error(t('registerError'))
        }
      })
      .catch((error) =>
        toast.error(error?.response?.data?.message ?? t('registerError'))
      )

    setDisabled(false)
  }

  return (
    <div className="login-card login-dark">
      <div>
        <div>
          <CommonLogo alignLogo={alignLogo} />
        </div>
        <div className="login-main">
          <form
            className="theme-form"
            noValidate
            autoComplete="off"
            onSubmit={handleSubmit(onSubmit)}
          >
            <h4 suppressHydrationWarning>{t('createAccount')}</h4>
            <p>{t('resetPasswordInfo')}</p>
            <FormGroup>
              <Row className="g-2">
                <Col xs={6}>
                  <Label for="name" className="col-form-label required pt-0">
                    {t('name')}
                  </Label>
                  <Controller
                    name="name"
                    control={control}
                    disabled={disabled}
                    rules={{ required: true }}
                    render={({ field: { name, ...rest } }) => (
                      <Input
                        id={name}
                        placeholder={t('namePlaceholder')}
                        autoComplete="on"
                        invalid={Boolean(errors.name)}
                        {...rest}
                      />
                    )}
                  />
                  <FormFeedback>
                    {errors.name && errors.name.message}
                  </FormFeedback>
                </Col>
                <Col xs={6}>
                  <Label
                    for="systemName"
                    className="col-form-label required pt-0"
                  >
                    {t('systemName')}
                  </Label>
                  <Controller
                    name="systemName"
                    control={control}
                    disabled={disabled}
                    rules={{ required: true }}
                    render={({ field: { name, ...rest } }) => (
                      <Input
                        id={name}
                        placeholder={t('systemNamePlaceholder')}
                        autoComplete="on"
                        invalid={Boolean(errors.systemName)}
                        {...rest}
                      />
                    )}
                  />
                  <FormFeedback>
                    {errors.systemName && errors.systemName.message}
                  </FormFeedback>
                </Col>
              </Row>
            </FormGroup>
            <FormGroup>
              <Label for="email" className="col-form-label required">
                {t('email')}
              </Label>
              <Controller
                name="email"
                control={control}
                disabled={disabled}
                rules={{ required: true }}
                render={({ field: { name, ...rest } }) => (
                  <Input
                    id={name}
                    type="email"
                    placeholder={t('emailPlaceholder')}
                    autoComplete="on"
                    invalid={Boolean(errors.email)}
                    {...rest}
                  />
                )}
              />
              <FormFeedback>
                {errors.email && errors.email.message}
              </FormFeedback>
            </FormGroup>
            <FormGroup>
              <Row className="g-2">
                <Col xs={6}>
                  <Label
                    for="password"
                    className="col-form-label required pt-0"
                  >
                    {t('password')}
                  </Label>
                  <div className="form-input position-relative">
                    <Controller
                      name="password"
                      control={control}
                      disabled={disabled}
                      rules={{ required: true }}
                      render={({ field: { name, ...rest } }) => (
                        <Input
                          id={name}
                          type={showPassword ? 'text' : 'password'}
                          autoComplete="off"
                          invalid={Boolean(errors.password)}
                          {...rest}
                        />
                      )}
                    />
                    <FormFeedback>
                      {errors.password && errors.password.message}
                    </FormFeedback>
                    <div className="show-hide">
                      <span
                        onClick={() => setShowPassword(!showPassword)}
                        className={!showPassword ? 'show' : ''}
                      >
                        {showPassword ? t('hide') : t('show')}
                      </span>
                    </div>
                  </div>
                </Col>
                <Col xs={6}>
                  <Label
                    for="passwordConfirmation"
                    className="col-form-label required pt-0"
                  >
                    {t('passwordConfirmation')}
                  </Label>
                  <div className="form-input position-relative">
                    <Controller
                      name="passwordConfirmation"
                      control={control}
                      disabled={disabled}
                      rules={{ required: true }}
                      render={({ field: { name, ...rest } }) => (
                        <Input
                          id={name}
                          type={showPasswordConfirmation ? 'text' : 'password'}
                          autoComplete="off"
                          invalid={Boolean(errors.passwordConfirmation)}
                          {...rest}
                        />
                      )}
                    />
                    <FormFeedback>
                      {errors.passwordConfirmation &&
                        errors.passwordConfirmation.message}
                    </FormFeedback>
                    <div className="show-hide">
                      <span
                        onClick={() =>
                          setShowPasswordConfirmation(!showPasswordConfirmation)
                        }
                        className={!showPasswordConfirmation ? 'show' : ''}
                      >
                        {showPasswordConfirmation ? t('hide') : t('show')}
                      </span>
                    </div>
                  </div>
                </Col>
              </Row>
            </FormGroup>
            <FormGroup className="mb-0 form-group">
              <div className="text-end mt-3">
                <Button
                  color="primary"
                  className="btn-block w-100"
                  type="submit"
                  disabled={disabled}
                >
                  {t('register')}
                </Button>
              </div>
              <p className="mt-4 mb-0 text-center">
                {t('alreadyHaveAnAccount')}
                <Link className="ms-2" href="/auth/login">
                  {t('signIn')}
                </Link>
              </p>
            </FormGroup>
          </form>
        </div>
      </div>
    </div>
  )
}
