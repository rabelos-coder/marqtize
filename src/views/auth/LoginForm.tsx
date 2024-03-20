'use client'

import { useMutation } from '@apollo/client'
import { yupResolver } from '@hookform/resolvers/yup'
import { useTranslations } from 'next-intl'
import { useEffect, useState } from 'react'
import { Controller, useForm } from 'react-hook-form'
import { FaFacebook, FaGoogle } from 'react-icons/fa'
import { toast } from 'react-toastify'
import { Button, FormFeedback, FormGroup, Input, Label } from 'reactstrap'
import * as yup from 'yup'

import { CommonLogo } from '@/components/common/CommonLogo'
import { EMAIL_REGEX, PASSWORD_STRENGTH_REGEX } from '@/configs'
import { IS_DEVELOPMENT, SERVER_URL } from '@/environment'
import { LOGIN } from '@/graphql/auth'
import { useAppDispatch, useAppSelector } from '@/hooks'
import { Link, useRouter } from '@/navigation'
import { setAuth } from '@/store/slices/authSlice'
import { setLoading } from '@/store/slices/themeSlice'
import { LoginInput } from '@/types/auth'

import { SpinnerBoxed } from '../../components/common/SpinnerBoxed'

type AuthProps = {
  host?: string
  alignLogo?: string
}

type FormData = {
  email: string
  password: string
}

const defaultValues = IS_DEVELOPMENT
  ? {
      email: 'amandasmith@me.com',
      password: 'abClK1@X',
    }
  : {
      email: '',
      password: '',
    }

export const LoginForm = ({ alignLogo }: AuthProps) => {
  const [disabled, setDisabled] = useState(false)
  const [rememberMe, setRememberMe] = useState(false)
  const [showPassword, setShowPassword] = useState(false)
  const [login] = useMutation(LOGIN, { fetchPolicy: 'no-cache' })

  const t = useTranslations()
  const dispatch = useAppDispatch()
  const { customer, loading } = useAppSelector((state) => state.customer)

  const schema = yup.object().shape({
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

  const router = useRouter()

  const onSubmit = async (form: FormData) => {
    setDisabled(true)

    const variables: LoginInput = {
      data: { email: form.email, password: form.password, rememberMe },
    }

    await login({ variables })
      .then(({ data }) => {
        if (data?.login) {
          const {
            user: { language },
          } = data.login
          toast.success(t('loginSuccess'))
          const locale = language.replace('_', '-').toLowerCase()
          dispatch(setAuth(data.login))

          router.replace('/backend', { locale })
          router.refresh()
        } else {
          toast.error(t('loginError'))
        }
      })
      .catch((error) => toast.error(error?.message ?? t('loginError')))

    setDisabled(false)
  }

  useEffect(() => {
    dispatch(setLoading(false))
  }, [dispatch])

  return loading ? (
    <SpinnerBoxed type="grow" />
  ) : (
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
            <h4 suppressHydrationWarning>
              {customer?.tradingName
                ? t('signInToAccountName', {
                    name: customer.tradingName,
                  })
                : t('signInToAccount')}
            </h4>
            <p>{t('signInToAccountInfo')}</p>
            <FormGroup>
              <Label htmlFor="email" className="col-form-label">
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
                    autoFocus
                    autoComplete="on"
                    placeholder={t('emailPlaceholder')}
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
              <Label htmlFor="password" className="col-form-label">
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
            </FormGroup>
            <FormGroup className="mb-0 form-group">
              <div className="checkbox p-0">
                <Input
                  id="rememberMe"
                  type="checkbox"
                  onChange={() => setRememberMe(!rememberMe)}
                />
                <Label className="text-muted" htmlFor="rememberMe">
                  {t('rememberMe')}
                </Label>
              </div>
              <Link className="link" href="/auth/forgot-password">
                {t('forgotYourPassword')}
              </Link>
              <div className="text-end mt-3">
                <Button
                  color="primary"
                  className="btn-block w-100"
                  type="submit"
                  disabled={disabled}
                >
                  {t('signIn')}
                </Button>
              </div>
            </FormGroup>
            <h6 className="text-muted mt-4 or">{t('orSignInWith')}</h6>
            <div className="social mt-4">
              <div className="btn-showcase">
                <Link
                  className="btn btn-light"
                  href={`${SERVER_URL}/auth/facebook`}
                  rel="noreferrer"
                  title={t('facebook')}
                >
                  <FaFacebook className="social-icon txt-facebook" />
                  {t('facebook')}
                </Link>
                <Link
                  className="btn btn-light"
                  href={`${SERVER_URL}/auth/google`}
                  rel="noreferrer"
                  title={t('google')}
                >
                  <FaGoogle className="social-icon txt-google" />
                  {t('google')}
                </Link>
              </div>
            </div>
            <p className="mt-4 mb-0 text-center">
              {t('dontHaveAnAccount')}
              <Link className="ms-2" href="/auth/register">
                {t('createAccount')}
              </Link>
            </p>
          </form>
        </div>
      </div>
    </div>
  )
}
