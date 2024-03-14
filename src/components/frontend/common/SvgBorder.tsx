import GeometricForm from "../../../app/assets/svg/geometric-form.svg";

type SvgBorderProps = {
  className?: string;
};

export const SvgBorder = ({ className }: SvgBorderProps) => {
  return (
    <div className={`svg-border-rounded ${className}`}>
      <GeometricForm />
    </div>
  );
};
