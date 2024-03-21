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

package de.markusbordihn.easynpc.client.screen.configuration.position;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.components.Checkbox;
import de.markusbordihn.easynpc.client.screen.components.Text;
import de.markusbordihn.easynpc.client.screen.components.TextButton;
import de.markusbordihn.easynpc.client.screen.components.TextField;
import de.markusbordihn.easynpc.data.attribute.EntityAttribute;
import de.markusbordihn.easynpc.entity.easynpc.data.AttributeData;
import de.markusbordihn.easynpc.menu.configuration.position.DefaultPositionConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkMessageHandler;
import net.minecraft.client.gui.GuiGraphics;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.network.chat.Component;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.phys.Vec3;
import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;
import net.minecraftforge.client.event.ScreenEvent.BackgroundRendered;

@OnlyIn(Dist.CLIENT)
public class DefaultPositionConfigurationScreen
    extends PositionConfigurationScreen<DefaultPositionConfigurationMenu> {

  // Default Values
  private static final float POSITION_STEPS = 0.5f;

  // Position Coordinates EditBoxes
  protected EditBox positionXBox;
  protected EditBox positionYBox;
  protected EditBox positionZBox;
  protected Checkbox positionFreefallCheckbox;

  // Position Coordinates
  protected double positionX = 0.0D;
  protected double positionY = 0.0D;
  protected double positionZ = 0.0D;

  // Position Coordinates Buttons
  protected Button positionXMinusButton;
  protected Button positionXPlusButton;
  protected Button positionYMinusButton;
  protected Button positionYPlusButton;
  protected Button positionZMinusButton;
  protected Button positionZPlusButton;

  public DefaultPositionConfigurationScreen(
      DefaultPositionConfigurationMenu menu, Inventory inventory, Component component) {
    super(menu, inventory, component);
  }

  @Override
  public void init() {
    super.init();

    // Default button stats
    this.defaultPositionButton.active = false;

    // Position Coordinates
    Vec3 entityPosition = this.easyNPC.getEntity().position();
    this.positionX = entityPosition.x;
    this.positionY = entityPosition.y;
    this.positionZ = entityPosition.z;

    // X Position
    this.positionXBox =
        this.addRenderableWidget(
            new TextField(this.font, this.contentLeftPos + 15, this.contentTopPos + 5, 60));
    this.positionXBox.setMaxLength(8);
    this.positionXBox.setValue(String.valueOf(this.positionX));
    this.positionXBox.setResponder(
        consumer -> {
          Double newPositionX = getDoubleValue(this.positionXBox.getValue());
          if (newPositionX != null) {
            this.positionX = newPositionX;
            NetworkMessageHandler.positionChange(
                this.uuid, new Vec3(this.positionX, this.positionY, this.positionZ));
          }
        });
    this.positionXMinusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionXBox.getX() - 15,
                this.positionXBox.getY(),
                15,
                "-",
                button -> {
                  this.positionX -= POSITION_STEPS;
                  this.positionXBox.setValue(String.valueOf(this.positionX));
                }));
    this.positionXPlusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionXBox.getX() + this.positionXBox.getWidth() + 1,
                this.positionXBox.getY(),
                15,
                "+",
                button -> {
                  this.positionX += POSITION_STEPS;
                  this.positionXBox.setValue(String.valueOf(this.positionX));
                }));

    // Y Position
    this.positionYBox =
        this.addRenderableWidget(
            new TextField(this.font, this.contentLeftPos + 111, this.contentTopPos + 5, 60));
    this.positionYBox.setMaxLength(8);
    this.positionYBox.setValue(String.valueOf(this.positionY));
    this.positionYBox.setResponder(
        consumer -> {
          Double newPositionY = getDoubleValue(this.positionYBox.getValue());
          if (newPositionY != null) {
            this.positionY = newPositionY;
            NetworkMessageHandler.positionChange(
                this.uuid, new Vec3(this.positionX, this.positionY, this.positionZ));
          }
        });
    this.positionYMinusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionYBox.getX() - 15,
                this.positionYBox.getY(),
                15,
                Component.literal("-"),
                button -> {
                  this.positionY -= POSITION_STEPS;
                  this.positionYBox.setValue(String.valueOf(this.positionY));
                }));
    this.positionYPlusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionYBox.getX() + this.positionYBox.getWidth() + 1,
                this.positionYBox.getY(),
                15,
                Component.literal("+"),
                button -> {
                  this.positionY += POSITION_STEPS;
                  this.positionYBox.setValue(String.valueOf(this.positionY));
                }));

    // Z Position
    this.positionZBox =
        this.addRenderableWidget(
            new TextField(this.font, this.contentLeftPos + 207, this.contentTopPos + 5, 60));
    this.positionZBox.setMaxLength(8);
    this.positionZBox.setValue(String.valueOf(this.positionZ));
    this.positionZBox.setResponder(
        consumer -> {
          Double newPositionZ = getDoubleValue(this.positionZBox.getValue());
          if (newPositionZ != null) {
            this.positionZ = newPositionZ;
            NetworkMessageHandler.positionChange(
                this.uuid, new Vec3(this.positionX, this.positionY, this.positionZ));
          }
        });
    this.positionZMinusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionZBox.getX() - 15,
                this.positionZBox.getY(),
                15,
                Component.literal("-"),
                button -> {
                  this.positionZ -= POSITION_STEPS;
                  this.positionZBox.setValue(String.valueOf(this.positionZ));
                }));
    this.positionZPlusButton =
        this.addRenderableWidget(
            new TextButton(
                this.positionZBox.getX() + this.positionZBox.getWidth() + 1,
                this.positionZBox.getY(),
                15,
                Component.literal("+"),
                button -> {
                  this.positionZ += POSITION_STEPS;
                  this.positionZBox.setValue(String.valueOf(this.positionZ));
                }));

    // Freefall Checkbox
    AttributeData<?> attributeData = this.easyNPC.getEasyNPCAttributeData();
    this.positionFreefallCheckbox =
        this.addRenderableWidget(
            new Checkbox(
                this.contentLeftPos + 200,
                this.topPos + 18,
                "free_fall",
                attributeData.getAttributeFreefall(),
                checkbox ->
                    NetworkMessageHandler.entityAttributeChange(
                        uuid, EntityAttribute.FREEFALL, checkbox.selected())));
  }

  @Override
  public void render(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    super.render(guiGraphics, x, y, partialTicks);

    // Position Text
    Text.drawString(
        guiGraphics,
        this.font,
        "Position X",
        this.positionXBox.getX() + 5,
        this.positionXBox.getY() - 10);
    Text.drawString(
        guiGraphics,
        this.font,
        "Position Y",
        this.positionYBox.getX() + 5,
        this.positionYBox.getY() - 10);
    Text.drawString(
        guiGraphics,
        this.font,
        "Position Z",
        this.positionZBox.getX() + 5,
        this.positionZBox.getY() - 10);
  }

  @Override
  public void renderBackground(GuiGraphics guiGraphics, int x, int y, float partialTicks) {
    // Use a more transparent background than the default.
    if (this.clientLevel != null) {
      guiGraphics.fillGradient(0, 0, this.width, this.height, 0x55000000, 0x55000000);
      net.minecraftforge.common.MinecraftForge.EVENT_BUS.post(
          new BackgroundRendered(this, guiGraphics));
    }
    this.renderBg(guiGraphics, partialTicks, x, y);
  }

  @Override
  protected void renderBg(GuiGraphics guiGraphics, float partialTicks, int mouseX, int mouseY) {
    // Main screen: top left
    guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos, 0, 0, 250, 70);

    // Main screen: top right
    guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 243, topPos, 195, 0, 57, 70);

    // Main screen: bottom left
    guiGraphics.blit(Constants.TEXTURE_DEMO_BACKGROUND, leftPos, topPos + 22, 0, 115, 250, 60);

    // Main screen: bottom right
    guiGraphics.blit(
        Constants.TEXTURE_DEMO_BACKGROUND, leftPos + 243, topPos + 22, 195, 115, 57, 60);
  }
}
