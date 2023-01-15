/**
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

package de.markusbordihn.easynpc.client.screen.configuration;

import java.util.UUID;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Inventory;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.menu.configuration.MainConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkHandler;

public class MainConfigurationScreen extends AbstractContainerScreen<MainConfigurationMenu> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final EasyNPCEntity entity;
  protected final UUID uuid;

  private Button editDialogButton = null;
  private EditBox nameBox;
  private float xMouse;
  private float yMouse;

  public MainConfigurationScreen(MainConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
    this.entity = menu.getEntity();
    this.uuid = this.entity.getUUID();
  }

  private void saveName() {
    String value = this.nameBox.getValue();
    if (value != null && !value.isBlank()) {
      NetworkHandler.nameChange(this.uuid, value);
    }
  }

  @Override
  public void init() {
    super.init();

    // Default stats
    this.imageHeight = 220;
    this.imageWidth = 275;

    // Basic Position
    this.titleLabelX = 60;
    this.titleLabelY = 6;
    this.topPos = (this.height - this.imageHeight) / 2;
    this.leftPos = (this.width - this.imageWidth) / 2;

    // Name
    this.nameBox = new EditBox(this.font, this.leftPos + 7, this.topPos + 32, 261, 16,
        new TranslatableComponent("Name"));
    this.nameBox.setMaxLength(32);
    this.nameBox.setValue(
        this.entity.hasCustomName() ? this.entity.getCustomName().getString() : "My Easy NPC");
    this.addRenderableWidget(this.nameBox);

    // Dialog
    this.editDialogButton = this.addRenderableWidget(new Button(this.leftPos + 60, this.topPos + 67,
        80, 20, new TranslatableComponent("Edit Dialog"), onPress -> {
          log.info("Edit dialog ...");
        }));

    // Skins

    // Actions
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    super.render(poseStack, x, y, partialTicks);
    this.xMouse = x;
    this.yMouse = y;

    // Name
    this.font.draw(poseStack, new TextComponent("Name"), this.leftPos + 7f, this.topPos + 20f,
        4210752);

    // Dialog
    this.font.draw(poseStack, new TextComponent("Dialog"), this.leftPos + 7f, this.topPos + 55f,
        4210752);


    // Skins
    this.font.draw(poseStack, new TextComponent("Skins"), this.leftPos + 7f, this.topPos + 90f,
        4210752);

    // Actions
    this.font.draw(poseStack, new TextComponent("Actions"), this.leftPos + 7f, this.topPos + 180f,
        4210752);
  }

  @Override
  protected void renderLabels(PoseStack poseStack, int x, int y) {
    this.font.draw(poseStack, this.title, this.titleLabelX, this.titleLabelY, 4210752);
  }

  @Override
  protected void renderBg(PoseStack poseStack, float partialTicks, int mouseX, int mouseY) {
    RenderSystem.setShader(GameRenderer::getPositionTexShader);
    RenderSystem.setShaderColor(1.0F, 1.0F, 1.0F, 1.0F);
    RenderSystem.setShaderTexture(0, Constants.TEXTURE_DEMO_BACKGROUND);

    // Main screen (+50px in height)
    this.blit(poseStack, leftPos, topPos, 0, 0, 250, 170);
    this.blit(poseStack, leftPos + 243, topPos, 215, 0, 35, 170);

    int expandedHeight = 50;
    this.blit(poseStack, leftPos, topPos + expandedHeight + 5, 0, 5, 250, 170);
    this.blit(poseStack, leftPos + 243, topPos + expandedHeight + 5, 215, 5, 35, 170);
  }

  @Override
  public boolean keyPressed(int keyCode, int unused1, int unused2) {
    if (keyCode != 257 && keyCode != 335 && keyCode != 69) {
      return super.keyPressed(keyCode, unused1, unused2);
    } else if (keyCode == 257 || keyCode == 335) {
      // Save name on enter or return.
      this.saveName();
      return true;
    } else {
      return true;
    }
  }

}
