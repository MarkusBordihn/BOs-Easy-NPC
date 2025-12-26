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

import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.RangeSliderButton;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.configui.client.renderer.screen.EntityConfigScreenRenderer;
import de.markusbordihn.easynpc.configui.menu.configuration.ConfigurationMenu;
import de.markusbordihn.easynpc.configui.network.NetworkMessageHandlerManager;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.render.EntityRenderConfig;
import java.util.EnumMap;
import java.util.Set;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;

public class AdvancedPoseConfigurationScreen<T extends ConfigurationMenu>
    extends PoseConfigurationScreen<T> {

  private final EnumMap<ModelPartType, RangeSliderButton> sliders =
      new EnumMap<>(ModelPartType.class);

  public AdvancedPoseConfigurationScreen(T menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  private RangeSliderButton createVisibilityRotationPositionSlider(
      int left, int top, ModelPartType modelPartType, String label) {

    // Model Part Rotation
    RangeSliderButton sliderRotationButtonX = createRotationSlider(left, top, modelPartType, label);

    // Model Part Position
    RangeSliderButton sliderPositionButtonX =
        createPositionSliderCompact(
            left, top + sliderRotationButtonX.getHeight(), modelPartType, label);

    // Model Part Visibility
    boolean modelPartVisibility = this.modelData.getModelPartVisibility(modelPartType);
    this.addRenderableWidget(
        new Checkbox(
            sliderRotationButtonX.getX() + 3,
            top - sliderPositionButtonX.getHeight(),
            "",
            modelPartVisibility,
            checkbox ->
                NetworkMessageHandlerManager.getServerHandler()
                    .modelVisibilityChange(
                        this.getEasyNPCUUID(), modelPartType, checkbox.selected())));

    return sliderRotationButtonX;
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.advancedPoseButton.active = false;

    // Position and size
    int sliderTopPos = this.contentTopPos + 26;
    int sliderLeftPos = this.contentLeftPos - 3;
    int sliderLeftSpace = 200;
    int sliderTopSpace = 60;

    // Model parts
    Set<ModelPartType> modelPartTypes = this.modelData.getModelType().getPrimaryModelParts();
    int partsOnRow = 0;
    for (ModelPartType modelPartType : modelPartTypes) {
      RangeSliderButton slider =
          createVisibilityRotationPositionSlider(
              sliderLeftPos, sliderTopPos, modelPartType, modelPartType.name().toLowerCase());
      sliders.put(modelPartType, slider);

      sliderLeftPos += sliderLeftSpace;
      partsOnRow++;
      if (partsOnRow >= 2) {
        partsOnRow = 0;
        sliderLeftPos = this.contentLeftPos - 3;
        sliderTopPos += sliderTopSpace;
      }
    }

    // Animation Behavior Button
    this.addRenderableWidget(
        this.createAnimationBehaviorButton(this.contentLeftPos + 118, this.bottomPos - 26));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Avatar
    EntityConfigScreenRenderer.renderEntity(
        guiGraphics,
        this.getEasyNPC(),
        EntityRenderConfig.guiScaled(this.contentLeftPos + 157, this.contentTopPos + 110, 45),
        this.xMouse,
        this.yMouse);

    // Model Part texts
    for (ModelPartType modelPartType : sliders.keySet()) {
      RangeSliderButton slider = sliders.get(modelPartType);
      if (slider != null) {
        Text.drawConfigString(
            guiGraphics,
            this.font,
            "pose." + modelPartType.name().toLowerCase(),
            slider.getX() + 20,
            slider.getY() - 12);
      }
    }
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    super.renderBg(guiGraphics, partialTicks, mouseX, mouseY);

    // Entity
    int backgroundTopPos = this.contentTopPos + 30;
    guiGraphics.fill(
        this.contentLeftPos + 109,
        backgroundTopPos,
        this.contentLeftPos + 206,
        this.contentTopPos + 178,
        0xff000000);
    guiGraphics.fill(
        this.contentLeftPos + 110,
        backgroundTopPos + 1,
        this.contentLeftPos + 205,
        this.contentTopPos + 177,
        0xffaaaaaa);

    // Animation Text
    Text.drawConfigString(
        guiGraphics, this.font, "animation", this.contentLeftPos + 134, this.bottomPos - 37);
  }
}
