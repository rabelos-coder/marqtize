"use client";

import { useMutation } from "@apollo/client";
import { yupResolver } from "@hookform/resolvers/yup";
import { trim } from "lodash";
import { useTranslations } from "next-intl";
import { Controller, useForm } from "react-hook-form";
import Select from "react-select";
import { toast } from "react-toastify";
import {
  Button,
  CardBody,
  CardFooter,
  CardHeader,
  Col,
  FormFeedback,
  FormGroup,
  Input,
  Label,
  Row,
} from "reactstrap";
import * as yup from "yup";

import { UPDATE_PROFILE } from "@/graphql/auth";
import { useAppDispatch } from "@/hooks";
import { usePathname, useRouter } from "@/navigation";
import { setLanguage, setTimezone, setUser } from "@/store/slices/authSlice";
import { EditProfileProps } from "@/types/auth";
import { Upload } from "@/types/common";

type FormData = {
  name: string;
  systemName: string;
  language: string;
  timezoneId: string;
  removeImage?: boolean;
  imageFile?: Upload;
};

const EditProfileForm = ({ user, timezones, languages }: EditProfileProps) => {
  const t = useTranslations();
  const [updateProfile, { loading }] = useMutation(UPDATE_PROFILE, {
    fetchPolicy: "no-cache",
  });

  const dispatch = useAppDispatch();
  const router = useRouter();
  const pathname = usePathname();

  const schema = yup.object().shape({
    name: yup.string().required(t("invalidNameRequired")),
    systemName: yup.string().required(t("invalidSystemNameRequired")),
    language: yup.string().required(t("invalidLanguageRequired")),
    timezoneId: yup.string().required(t("invalidTimezoneRequired")),
  });

  const defaultValues = Object.assign({ passwordConfirmation: "" }, user);

  const onSubmit = async (form: FormData) => {
    await updateProfile({
      variables: {
        data: {
          name: form.name,
          systemName: form.systemName,
          language: form.language,
          timezoneId: form.timezoneId,
        },
      },
    })
      .then(({ data }) => {
        if (data?.updateProfile) {
          toast.success(t("profileUpdatedSuccess"));
          dispatch(setUser(data.updateProfile));
          dispatch(setLanguage(data.updateProfile.language));
          dispatch(setTimezone(data.updateProfile.timezone.code));
          router.push(pathname, { locale: data.updateProfile.language });
        } else {
          toast.error(t("profileUpdatedError"));
        }
      })
      .catch((error) =>
        toast.error(error?.message ?? t("profileUpdatedError"))
      );
  };

  const {
    control,
    handleSubmit,
    formState: { errors },
  } = useForm({
    defaultValues,
    mode: "onBlur",
    resolver: yupResolver(schema),
  });

  return (
    <Col xl={8}>
      <form className="card" noValidate onSubmit={handleSubmit(onSubmit)}>
        <CardHeader>
          <h4 className="card-title mb-0">{t("editProfile")}</h4>
        </CardHeader>
        <CardBody>
          <Row>
            <Col sm={6} md={6}>
              <FormGroup>
                <Label htmlFor="name">{t("name")}</Label>
                <Controller
                  name="name"
                  disabled={loading}
                  control={control}
                  rules={{ required: true }}
                  render={({ field: { name, ...rest } }) => (
                    <Input
                      id={name}
                      autoFocus
                      autoComplete="on"
                      placeholder={t("namePlaceholder")}
                      invalid={Boolean(errors.name)}
                      {...rest}
                    />
                  )}
                />
                <FormFeedback>
                  {errors.name && errors.name.message}
                </FormFeedback>
              </FormGroup>
            </Col>
            <Col sm={6} md={6}>
              <FormGroup>
                <Label htmlFor="systemName">{t("systemName")}</Label>
                <Controller
                  name="systemName"
                  control={control}
                  disabled={loading}
                  rules={{ required: true }}
                  render={({ field: { name, ...rest } }) => (
                    <Input
                      id={name}
                      placeholder={t("systemNamePlaceholder")}
                      autoComplete="on"
                      invalid={Boolean(errors.systemName)}
                      {...rest}
                    />
                  )}
                />
                <FormFeedback>
                  {errors.systemName && errors.systemName.message}
                </FormFeedback>
              </FormGroup>
            </Col>
            <Col md={12}>
              <FormGroup>
                <Label>{t("language")}</Label>
                <Controller
                  name="language"
                  control={control}
                  disabled={loading}
                  rules={{ required: true }}
                  render={({
                    field: { name, onChange, value, disabled, ref },
                  }) => (
                    <div
                      className={`select2-input ${errors.language && errors.language.message ? "has-error" : ""}`}
                    >
                      <Select
                        ref={ref}
                        name={name}
                        className="react-select-container"
                        classNamePrefix="react-select"
                        isDisabled={disabled ?? false}
                        onChange={(newValue) => onChange(newValue?.value ?? "")}
                        value={languages.find(
                          (option) => option.value === value
                        )}
                        options={
                          [{ label: t("selectOne"), value: "" }].concat(
                            languages as any
                          ) as any
                        }
                        noOptionsMessage={({ inputValue }) =>
                          !trim(inputValue as string) ? "" : t("noResultsFound")
                        }
                      />
                      <input
                        type="hidden"
                        className={
                          errors.language && errors.language.message
                            ? "is-invalid"
                            : ""
                        }
                        value={value}
                      />
                      <FormFeedback>
                        {errors.language && errors.language.message}
                      </FormFeedback>
                    </div>
                  )}
                />
              </FormGroup>
            </Col>
            <Col md={12}>
              <Label>{t("timezone")}</Label>
              <Controller
                name="timezoneId"
                control={control}
                disabled={loading}
                rules={{ required: true }}
                render={({
                  field: { name, onChange, value, disabled, ref },
                }) => (
                  <div
                    className={`select2-input ${errors.timezoneId && errors.timezoneId.message ? "has-error" : ""}`}
                  >
                    <Select
                      ref={ref}
                      name={name}
                      className="react-select-container"
                      classNamePrefix="react-select"
                      isDisabled={disabled ?? false}
                      onChange={(newValue) => onChange(newValue?.value ?? "")}
                      value={timezones.find((option) => option.value === value)}
                      options={
                        [{ label: t("selectOne"), value: "" }].concat(
                          timezones as any
                        ) as any
                      }
                      noOptionsMessage={({ inputValue }) =>
                        !trim(inputValue as string) ? "" : t("noResultsFound")
                      }
                    />
                    <input
                      type="hidden"
                      className={
                        errors.timezoneId && errors.timezoneId.message
                          ? "is-invalid"
                          : ""
                      }
                      value={value}
                    />
                    <FormFeedback>
                      {errors.timezoneId && errors.timezoneId.message}
                    </FormFeedback>
                  </div>
                )}
              />
            </Col>
          </Row>
        </CardBody>
        <CardFooter className="text-end">
          <Button color="primary" disabled={loading} type="submit">
            {t("save")}
          </Button>
        </CardFooter>
      </form>
    </Col>
  );
};

export default EditProfileForm;
