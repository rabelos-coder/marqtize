"use client";

import { yupResolver } from "@hookform/resolvers/yup";
import { useTranslations } from "next-intl";
import { useEffect, useState } from "react";
import { Controller, useForm } from "react-hook-form";
import { FaFacebook, FaGoogle, FaMicrosoft } from "react-icons/fa";
import { toast } from "react-toastify";
import { Button, FormFeedback, FormGroup, Input, Label } from "reactstrap";
import * as yup from "yup";

import { CommonLogo } from "@/components/common/CommonLogo";
import { EMAIL_REGEX, PASSWORD_STRENGTH_REGEX } from "@/configs";
import { IS_DEVELOPMENT } from "@/environment";
import { useAppDispatch, useAppSelector, useAuth } from "@/hooks";
import { Link, useRouter } from "@/navigation";
import { resetError } from "@/store/slices/authSlice";
import { fetchCustomer } from "@/store/slices/customerSlice";

import { SpinnerBoxed } from "../common/SpinnerBoxed";

type AuthProps = {
  host: string;
  alignLogo?: string;
};

type FormData = {
  email: string;
  password: string;
};

const defaultValues = IS_DEVELOPMENT
  ? {
      email: "amandasmith@me.com",
      password: "abClK1@X",
    }
  : {
      email: "",
      password: "",
    };

export const LoginForm = ({ host, alignLogo }: AuthProps) => {
  const [rememberMe, setRememberMe] = useState(false);
  const [showPassword, setShowPassword] = useState(false);

  const t = useTranslations("translations");
  const dispatch = useAppDispatch();
  const { signIn, loading, error, language, isLoggedIn } = useAuth();
  const { customer, loading: customerLoading } = useAppSelector(
    (state) => state.customer
  );

  const schema = yup.object().shape({
    email: yup
      .string()
      .email(t("invalidEmailFormat"))
      .required(t("invalidEmailRequired"))
      .matches(new RegExp(EMAIL_REGEX), t("invalidEmailFormat")),
    password: yup
      .string()
      .trim()
      .required(t("invalidPasswordRequired"))
      .matches(
        new RegExp(PASSWORD_STRENGTH_REGEX),
        t("invalidPasswordStrength")
      ),
  });

  const {
    control,
    handleSubmit,
    formState: { errors },
  } = useForm({
    defaultValues,
    mode: "onBlur",
    resolver: yupResolver(schema),
  });

  const router = useRouter();

  const onSubmit = async (data: FormData) => {
    await signIn({ data: { ...data, rememberMe } });
  };

  useEffect(() => {
    dispatch(resetError());
    dispatch(fetchCustomer(host));
    if (isLoggedIn && language && !loading) {
      router.replace("/backend", { locale: language });
      router.refresh();
    }
    if (error) {
      toast.error(error);
    }
  }, [dispatch, error, host, isLoggedIn, language, loading, router]);

  return customerLoading ? (
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
                ? t("signInToAccountName", {
                    name: customer.tradingName,
                  })
                : t("signInToAccount")}
            </h4>
            <p>{t("signInToAccountInfo")}</p>
            <FormGroup>
              <Label htmlFor="email" className="col-form-label">
                {t("email")}
              </Label>
              <Controller
                name="email"
                control={control}
                rules={{ required: true }}
                render={({
                  field: { name, value, onChange, onBlur, ...rest },
                }) => (
                  <Input
                    id={name}
                    type="email"
                    autoFocus
                    placeholder={t("emailPlaceholder")}
                    value={value}
                    onBlur={onBlur}
                    onChange={onChange}
                    valid={!Boolean(errors.email)}
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
                {t("password")}
              </Label>
              <div className="form-input position-relative">
                <Controller
                  name="password"
                  control={control}
                  rules={{ required: true }}
                  render={({
                    field: { name, value, onChange, onBlur, ...rest },
                  }) => (
                    <Input
                      id={name}
                      type={showPassword ? "text" : "password"}
                      placeholder="*********"
                      value={value}
                      onBlur={onBlur}
                      onChange={onChange}
                      valid={!Boolean(errors.password)}
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
                    className={!showPassword ? "show" : ""}
                  >
                    {showPassword ? t("hide") : t("show")}
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
                  {t("rememberMe")}
                </Label>
              </div>
              <Link className="link" href="/auth/forgot-password">
                {t("forgotYourPassword")}
              </Link>
              <div className="text-end mt-3">
                <Button
                  color="primary"
                  className="btn-block w-100"
                  type="submit"
                  disabled={loading}
                >
                  {t("signIn")}
                </Button>
              </div>
            </FormGroup>
            <h6 className="text-muted mt-4 or">{t("orSignInWith")}</h6>
            <div className="social mt-4">
              <div className="btn-showcase">
                <Link
                  className="btn btn-light"
                  href="https://www.google.com/"
                  target="_blank"
                  rel="noreferrer"
                >
                  <FaGoogle className="social-icon txt-google" />
                  {t("google")}
                </Link>
                <Link
                  className="btn btn-light"
                  href="https://www.microsoft.com/"
                  target="_blank"
                  rel="noreferrer"
                >
                  <FaMicrosoft className="social-icon txt-microsoft" />
                  {t("microsoft")}
                </Link>
                <Link
                  className="btn btn-light"
                  href="https://www.facebook.com/"
                  target="_blank"
                  rel="noreferrer"
                >
                  <FaFacebook className="social-icon txt-facebook" />
                  {t("facebook")}
                </Link>
              </div>
            </div>
            <p className="mt-4 mb-0 text-center">
              {t("dontHaveAnAccount")}
              <Link className="ms-2" href="/auth/register">
                {t("createAccount")}
              </Link>
            </p>
          </form>
        </div>
      </div>
    </div>
  );
};
