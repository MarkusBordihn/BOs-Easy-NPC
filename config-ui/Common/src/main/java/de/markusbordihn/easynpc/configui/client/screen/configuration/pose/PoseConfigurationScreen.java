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
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class PoseConfigurationScreen<T extends ConfigurationMenu> extends ConfigurationScreen<T> {

  protected final ModelDataCapable<?> modelData;
  protected Button defaultPoseButton;
  protected Button basicPoseButton;
  protected Button advancedPoseButton;
  protected Button customPoseButton;

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

  protected RangeSliderButton createRotationSlider(
      int left, int top, ModelPartType modelPart, String label) {
    int sliderWidth = 34;
    int sliderHeight = 16;
    int sliderLeftPosition = left + 10;

    // Model Part Rotation
    CustomRotation modelPartRotation = this.modelData.getModelPartRotation(modelPart);
    RangeSliderButton sliderButtonX =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderLeftPosition,
                top,
                sliderWidth,
                sliderHeight,
                Math.toDegrees(modelPartRotation.x()),
                0,
                SliderButton.Type.DEGREE,
                false,
                slider -> {
                  CustomRotation currentModelPartRotation =
                      this.modelData.getModelPartRotation(modelPart);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelRotationChange(
                          this.getEasyNPCUUID(),
                          modelPart,
                          new CustomRotation(
                              (float) Math.toRadians(slider.getTargetValue()),
                              currentModelPartRotation.y(),
                              currentModelPartRotation.z()));
                }));
    RangeSliderButton sliderButtonY =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonX.getX() + sliderButtonX.getWidth(),
                top,
                sliderWidth,
                sliderHeight,
                Math.toDegrees(modelPartRotation.y()),
                0,
                SliderButton.Type.DEGREE,
                false,
                slider -> {
                  CustomRotation currentModelPartRotation =
                      this.modelData.getModelPartRotation(modelPart);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelRotationChange(
                          this.getEasyNPCUUID(),
                          modelPart,
                          new CustomRotation(
                              currentModelPartRotation.x(),
                              (float) Math.toRadians(slider.getTargetValue()),
                              currentModelPartRotation.z()));
                }));
    RangeSliderButton sliderButtonZ =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonY.getX() + sliderButtonY.getWidth(),
                top,
                sliderWidth,
                sliderHeight,
                Math.toDegrees(modelPartRotation.z()),
                0,
                SliderButton.Type.DEGREE,
                false,
                slider -> {
                  CustomRotation currentModelPartRotation =
                      this.modelData.getModelPartRotation(modelPart);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelRotationChange(
                          this.getEasyNPCUUID(),
                          modelPart,
                          new CustomRotation(
                              currentModelPartRotation.x(),
                              currentModelPartRotation.y(),
                              (float) Math.toRadians(slider.getTargetValue())));
                }));

    // Slider Edit / Done Button
    this.addRenderableWidget(
        new TextButton(
            left,
            top,
            10,
            RangeSliderButton.EDIT_TEXT,
            button -> {
              if (button.getMessage() == RangeSliderButton.EDIT_TEXT) {
                sliderButtonX.showTextField();
                sliderButtonY.showTextField();
                sliderButtonZ.showTextField();
                button.setMessage(RangeSliderButton.DONE_TEXT);
              } else {
                sliderButtonX.showSliderButton();
                sliderButtonY.showSliderButton();
                sliderButtonZ.showSliderButton();
                button.setMessage(RangeSliderButton.EDIT_TEXT);
              }
            }));

    // Slider reset button
    int resetButtonLeftPosition = sliderButtonZ.getX() + sliderButtonZ.getWidth();
    this.addRenderableWidget(
        new TextButton(
            resetButtonLeftPosition,
            top,
            10,
            TextComponent.getText("↺"),
            button -> {
              sliderButtonX.reset();
              sliderButtonY.reset();
              sliderButtonZ.reset();
              NetworkMessageHandlerManager.getServerHandler()
                  .modelRotationChange(this.getEasyNPCUUID(), modelPart, CustomRotation.DEFAULT);
            }));

    return sliderButtonX;
  }

  protected RangeSliderButton createPositionSliderCompact(
      int left, int top, ModelPartType modelPartType, String label) {
    return createPositionSlider(left, top, modelPartType, label, true);
  }

  protected RangeSliderButton createPositionSlider(
      int left, int top, ModelPartType modelPartType, String label, boolean compact) {
    int sliderWidth = 34;
    int sliderHeight = 16;
    int sliderLeftPosition = left + 10;

    // Model Part Position.
    CustomPosition modelPartPosition = this.modelData.getModelPartPosition(modelPartType);
    RangeSliderButton sliderButtonX =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderLeftPosition,
                top,
                sliderWidth,
                sliderHeight,
                modelPartPosition.x(),
                0,
                SliderButton.Type.POSITION,
                false,
                slider -> {
                  CustomPosition currentModelPartPosition =
                      this.modelData.getModelPartPosition(modelPartType);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelPositionChange(
                          this.getEasyNPCUUID(),
                          modelPartType,
                          new CustomPosition(
                              slider.getTargetValue(),
                              currentModelPartPosition.y(),
                              currentModelPartPosition.z()));
                }));
    RangeSliderButton sliderButtonY =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonX.getX() + sliderButtonX.getWidth(),
                top,
                sliderWidth,
                sliderHeight,
                modelPartPosition.y(),
                0,
                SliderButton.Type.POSITION,
                false,
                slider -> {
                  CustomPosition currentModelPartPosition =
                      this.modelData.getModelPartPosition(modelPartType);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelPositionChange(
                          this.getEasyNPCUUID(),
                          modelPartType,
                          new CustomPosition(
                              currentModelPartPosition.x(),
                              slider.getTargetValue(),
                              currentModelPartPosition.z()));
                }));
    RangeSliderButton sliderButtonZ =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonY.getX() + sliderButtonY.getWidth(),
                top,
                sliderWidth,
                sliderHeight,
                modelPartPosition.z(),
                0,
                SliderButton.Type.POSITION,
                false,
                slider -> {
                  CustomPosition currentModelPartPosition =
                      this.modelData.getModelPartPosition(modelPartType);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelPositionChange(
                          this.getEasyNPCUUID(),
                          modelPartType,
                          new CustomPosition(
                              currentModelPartPosition.x(),
                              currentModelPartPosition.y(),
                              slider.getTargetValue()));
                }));

    // Slider Edit / Done Button
    this.addRenderableWidget(
        new TextButton(
            left,
            top,
            10,
            RangeSliderButton.EDIT_TEXT,
            button -> {
              if (button.getMessage() == RangeSliderButton.EDIT_TEXT) {
                sliderButtonX.showTextField();
                sliderButtonY.showTextField();
                sliderButtonZ.showTextField();
                button.setMessage(RangeSliderButton.DONE_TEXT);
              } else {
                sliderButtonX.showSliderButton();
                sliderButtonY.showSliderButton();
                sliderButtonZ.showSliderButton();
                button.setMessage(RangeSliderButton.EDIT_TEXT);
              }
            }));

    // Slider reset button
    int resetButtonLeftPosition = sliderButtonZ.getX() + sliderButtonZ.getWidth();
    this.addRenderableWidget(
        new TextButton(
            resetButtonLeftPosition,
            top,
            10,
            TextComponent.getText("↺"),
            button -> {
              sliderButtonX.reset();
              sliderButtonY.reset();
              sliderButtonZ.reset();
              NetworkMessageHandlerManager.getServerHandler()
                  .modelPositionChange(
                      this.getEasyNPCUUID(), modelPartType, CustomPosition.DEFAULT);
            }));

    return sliderButtonX;
  }

  protected RangeSliderButton createScaleSliderCompact(
      int left, int top, ModelPartType modelPartType, String label) {
    return createScaleSlider(left, top, modelPartType, label, true);
  }

  protected RangeSliderButton createScaleSlider(
      int left, int top, ModelPartType modelPartType, String label, boolean compact) {
    int sliderWidth = 34;
    int sliderHeight = 16;
    int sliderLeftScale = left + 10;

    // Model Part Scale.
    CustomScale modelPartScale = this.modelData.getModelPartScale(modelPartType);
    RangeSliderButton sliderButtonX =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderLeftScale,
                top,
                sliderWidth,
                sliderHeight,
                modelPartScale.x(),
                0,
                SliderButton.Type.SCALE,
                false,
                slider -> {
                  CustomScale currentModelPartScale =
                      this.modelData.getModelPartScale(modelPartType);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelScaleChange(
                          this.getEasyNPCUUID(),
                          modelPartType,
                          new CustomScale(
                              slider.getTargetValue(),
                              currentModelPartScale.y(),
                              currentModelPartScale.z()));
                }));
    RangeSliderButton sliderButtonY =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonX.getX() + sliderButtonX.getWidth(),
                top,
                sliderWidth,
                sliderHeight,
                modelPartScale.y(),
                0,
                SliderButton.Type.SCALE,
                false,
                slider -> {
                  CustomScale currentModelPartScale =
                      this.modelData.getModelPartScale(modelPartType);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelScaleChange(
                          this.getEasyNPCUUID(),
                          modelPartType,
                          new CustomScale(
                              currentModelPartScale.x(),
                              slider.getTargetValue(),
                              currentModelPartScale.z()));
                }));
    RangeSliderButton sliderButtonZ =
        this.addRenderableWidget(
            new RangeSliderButton(
                sliderButtonY.getX() + sliderButtonY.getWidth(),
                top,
                sliderWidth,
                sliderHeight,
                modelPartScale.z(),
                0,
                SliderButton.Type.SCALE,
                false,
                slider -> {
                  CustomScale currentModelPartScale =
                      this.modelData.getModelPartScale(modelPartType);
                  NetworkMessageHandlerManager.getServerHandler()
                      .modelScaleChange(
                          this.getEasyNPCUUID(),
                          modelPartType,
                          new CustomScale(
                              currentModelPartScale.x(),
                              currentModelPartScale.y(),
                              slider.getTargetValue()));
                }));

    // Slider Edit / Done Button
    this.addRenderableWidget(
        new TextButton(
            left,
            top,
            10,
            RangeSliderButton.EDIT_TEXT,
            button -> {
              if (button.getMessage() == RangeSliderButton.EDIT_TEXT) {
                sliderButtonX.showTextField();
                sliderButtonY.showTextField();
                sliderButtonZ.showTextField();
                button.setMessage(RangeSliderButton.DONE_TEXT);
              } else {
                sliderButtonX.showSliderButton();
                sliderButtonY.showSliderButton();
                sliderButtonZ.showSliderButton();
                button.setMessage(RangeSliderButton.EDIT_TEXT);
              }
            }));

    // Slider reset button
    int resetButtonLeftScale = sliderButtonZ.getX() + sliderButtonZ.getWidth();
    this.addRenderableWidget(
        new TextButton(
            resetButtonLeftScale,
            top,
            10,
            TextComponent.getText("↺"),
            button -> {
              sliderButtonX.reset();
              sliderButtonY.reset();
              sliderButtonZ.reset();
              NetworkMessageHandlerManager.getServerHandler()
                  .modelScaleChange(this.getEasyNPCUUID(), modelPartType, CustomScale.DEFAULT);
            }));

    return sliderButtonX;
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
  }
}
