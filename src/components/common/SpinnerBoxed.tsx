"use client";

type SpinnerBoxedProps = {
  type?: "border" | "grow";
  color?:
    | "primary"
    | "secondary"
    | "success"
    | "danger"
    | "warning"
    | "info"
    | "light"
    | "dark"
    | "default";
};

export const SpinnerBoxed = ({ type, color }: SpinnerBoxedProps) => {
  return (
    <div
      className="d-flex justify-content-center align-items-center"
      style={{ height: "100vh" }}
    >
      <div
        className={`spinner-${type ?? "border"} text-${color ?? "default"}`}
        role="status"
      ></div>
    </div>
  );
};
