/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.configui.client.screen.configuration.pose;

import de.markusbordihn.easynpc.client.screen.components.RangeSliderButton;
import de.markusbordihn.easynpc.client.screen.components.SliderButton;
import de.markusbordihn.easynpc.client.screen.components.SpinButton;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.configui.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.configui.client.screen.configuration.ConfigurationScreen;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.configuration.ConfigurationType;
import de.markusbordihn.easynpc.data.model.ModelAnimationBehavior;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.entity.easynpc.data.ModelDataCapable;
import de.markusbordihn.easynpc.network.components.TextComponent;
import java.util.LinkedHashSet;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.Tooltip;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class PoseConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  private static final int AXIS_X = 0;
  private static final int AXIS_Y = 1;
  private static final int AXIS_Z = 2;
  protected static boolean followCursor = true;
  protected final ModelDataCapable<?> modelData;
  protected Button defaultPoseButton;
  protected Button basicPoseButton;
  protected Button advancedPoseButton;
  protected Button customPoseButton;
  protected Checkbox lockRotationCheckbox;

  public PoseConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
    this.modelData = this.getEasyNPC().getEasyNPCModelData();
  }

  protected SpinButton<ModelAnimationBehavior> createAnimationBehaviorButton(int x, int y) {
    LinkedHashSet<ModelAnimationBehavior> values = new LinkedHashSet<>();
    values.add(ModelAnimationBehavior.SMART);
    values.add(ModelAnimationBehavior.DEFAULT);
    values.add(ModelAnimationBehavior.NONE);

    return this.addRenderableWidget(
        new SpinButton<>(
            x,
            y,
            80,
            16,
            values,
            this.modelData.getModelAnimationBehavior(),
            spinButton -> {
              ModelAnimationBehavior behavior = (ModelAnimationBehavior) spinButton.get();
              if (behavior != null) {
                NetworkMessageHandlerManager.getServerHandler()
                    .modelAnimationBehaviorChange(this.getEasyNPCUUID(), behavior);
              }
            }));
  }

  protected Checkbox createLockRotationCheckbox(int x, int y) {
    this.lockRotationCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                x,
                y,
                "lock_rotation",
                this.modelData.getModelRootData().rotation().locked(),
                checkbox ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .modelRotationChange(
                            this.getEasyNPCUUID(),
                            ModelPartType.ROOT,
                            this.modelData
                                .getModelRootData()
                                .rotation()
                                .withLocked(checkbox.selected()))));
    this.lockRotationCheckbox.setTooltip(
        Tooltip.create(TextComponent.getTranslatedConfigText("lock_rotation.tooltip")));
    return this.lockRotationCheckbox;
  }

  protected Button createFollowCursorToggleButton(int x, int y) {
    return this.addRenderableWidget(
        new TextButton(
            x,
            y,
            20,
            TextComponent.getText(followCursor ? "👁" : "⊙"),
            button -> {
              followCursor = !followCursor;
              button.setMessage(TextComponent.getText(followCursor ? "👁" : "⊙"));
            }));
  }

  protected RangeSliderButton createRotationSlider(int left, int top, ModelPartType modelPart) {
    CustomRotation modelPartRotation = this.modelData.getModelPartRotation(modelPart);
    return this.createAxisSlider(
        left,
        top,
        SliderButton.Type.DEGREE,
        new double[] {
          Math.toDegrees(modelPartRotation.x()),
          Math.toDegrees(modelPartRotation.y()),
          Math.toDegrees(modelPartRotation.z())
        },
        (axis, degreeValue) -> {
          CustomRotation currentRotation = this.modelData.getModelPartRotation(modelPart);
          float radianValue = (float) Math.toRadians(degreeValue);
          NetworkMessageHandlerManager.getServerHandler()
              .modelRotationChange(
                  this.getEasyNPCUUID(),
                  modelPart,
                  new CustomRotation(
                      axis == AXIS_X ? radianValue : currentRotation.x(),
                      axis == AXIS_Y ? radianValue : currentRotation.y(),
                      axis == AXIS_Z ? radianValue : currentRotation.z()));
        },
        () ->
            NetworkMessageHandlerManager.getServerHandler()
                .modelRotationChange(this.getEasyNPCUUID(), modelPart, CustomRotation.DEFAULT));
  }

  protected RangeSliderButton createPositionSlider(int left, int top, ModelPartType modelPartType) {
    CustomPosition modelPartPosition = this.modelData.getModelPartPosition(modelPartType);
    return this.createAxisSlider(
        left,
        top,
        SliderButton.Type.POSITION,
        new double[] {modelPartPosition.x(), modelPartPosition.y(), modelPartPosition.z()},
        (axis, value) -> {
          CustomPosition currentPosition = this.modelData.getModelPartPosition(modelPartType);
          NetworkMessageHandlerManager.getServerHandler()
              .modelPositionChange(
                  this.getEasyNPCUUID(),
                  modelPartType,
                  new CustomPosition(
                      axis == AXIS_X ? value : currentPosition.x(),
                      axis == AXIS_Y ? value : currentPosition.y(),
                      axis == AXIS_Z ? value : currentPosition.z()));
        },
        () ->
            NetworkMessageHandlerManager.getServerHandler()
                .modelPositionChange(this.getEasyNPCUUID(), modelPartType, CustomPosition.DEFAULT));
  }

  protected RangeSliderButton createScaleSlider(int left, int top, ModelPartType modelPartType) {
    CustomScale modelPartScale = this.modelData.getModelPartScale(modelPartType);
    return this.createAxisSlider(
        left,
        top,
        SliderButton.Type.SCALE,
        new double[] {modelPartScale.x(), modelPartScale.y(), modelPartScale.z()},
        (axis, value) -> {
          CustomScale currentScale = this.modelData.getModelPartScale(modelPartType);
          NetworkMessageHandlerManager.getServerHandler()
              .modelScaleChange(
                  this.getEasyNPCUUID(),
                  modelPartType,
                  new CustomScale(
                      axis == AXIS_X ? value : currentScale.x(),
                      axis == AXIS_Y ? value : currentScale.y(),
                      axis == AXIS_Z ? value : currentScale.z()));
        },
        () ->
            NetworkMessageHandlerManager.getServerHandler()
                .modelScaleChange(this.getEasyNPCUUID(), modelPartType, CustomScale.DEFAULT));
  }

  private RangeSliderButton createAxisSlider(
      int left,
      int top,
      SliderButton.Type sliderType,
      double[] axisValues,
      AxisValueSetter axisValueSetter,
      Runnable resetAction) {
    int sliderWidth = 34;
    int sliderHeight = 16;
    int sliderLeftPosition = left + 10;
    RangeSliderButton[] axisSliders = new RangeSliderButton[axisValues.length];

    for (int axis = 0; axis < axisValues.length; axis++) {
      int changedAxis = axis;
      RangeSliderButton axisSlider =
          this.addRenderableWidget(
              new RangeSliderButton(
                  sliderLeftPosition,
                  top,
                  sliderWidth,
                  sliderHeight,
                  axisValues[axis],
                  0,
                  sliderType,
                  false,
                  slider -> axisValueSetter.set(changedAxis, slider.getTargetValue())));
      axisSliders[axis] = axisSlider;
      sliderLeftPosition = axisSlider.getX() + axisSlider.getWidth();
    }

    // Slider Edit / Done Button
    this.addRenderableWidget(
        new TextButton(
            left,
            top,
            10,
            RangeSliderButton.EDIT_TEXT,
            button -> {
              boolean showTextField = button.getMessage() == RangeSliderButton.EDIT_TEXT;
              for (RangeSliderButton axisSlider : axisSliders) {
                if (showTextField) {
                  axisSlider.showTextField();
                } else {
                  axisSlider.showSliderButton();
                }
              }
              button.setMessage(
                  showTextField ? RangeSliderButton.DONE_TEXT : RangeSliderButton.EDIT_TEXT);
            }));

    // Slider reset button
    this.addRenderableWidget(
        new TextButton(
            sliderLeftPosition,
            top,
            10,
            TextComponent.getText("↺"),
            button -> {
              for (RangeSliderButton axisSlider : axisSliders) {
                axisSlider.reset();
              }
              resetAction.run();
            }));

    return axisSliders[0];
  }

  @Override
  public void init() {
    super.init();

    // Pose Types
    int poseButtonWidth = 74;
    this.defaultPoseButton =
        this.addRenderableWidget(
            new TextButton(
                this.buttonLeftPos,
                this.buttonTopPos,
                poseButtonWidth - 10,
                "default",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.DEFAULT_POSE)));

    this.basicPoseButton =
        this.addRenderableWidget(
            new TextButton(
                this.defaultPoseButton.getX() + this.defaultPoseButton.getWidth(),
                this.buttonTopPos,
                poseButtonWidth,
                "basic",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.BASIC_POSE)));

    this.advancedPoseButton =
        this.addRenderableWidget(
            new TextButton(
                basicPoseButton.getX() + basicPoseButton.getWidth(),
                this.buttonTopPos,
                poseButtonWidth + 10,
                "advanced",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(
                            this.getEasyNPCUUID(), ConfigurationType.ADVANCED_POSE)));

    this.customPoseButton =
        this.addRenderableWidget(
            new TextButton(
                advancedPoseButton.getX() + advancedPoseButton.getWidth(),
                this.buttonTopPos,
                poseButtonWidth + 10,
                "custom",
                button ->
                    NetworkMessageHandlerManager.getServerHandler()
                        .openConfiguration(this.getEasyNPCUUID(), ConfigurationType.CUSTOM_POSE)));

    this.defaultPoseButton.active = this.supportsConfigurationType(ConfigurationType.DEFAULT_POSE);
    this.basicPoseButton.active = this.supportsConfigurationType(ConfigurationType.BASIC_POSE);
    this.advancedPoseButton.active =
        this.supportsConfigurationType(ConfigurationType.ADVANCED_POSE);
  }

  protected float getPreviewRotationYaw(float mouseRelativeYaw) {
    return followCursor ? mouseRelativeYaw : 0.0f;
  }

  protected float getPreviewRotationPitch(float mouseRelativePitch) {
    return followCursor ? mouseRelativePitch : 0.0f;
  }

  @FunctionalInterface
  protected interface AxisValueSetter {
    void set(int axis, float value);
  }
}
