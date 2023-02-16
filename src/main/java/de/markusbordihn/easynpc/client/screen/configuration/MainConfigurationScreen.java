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

import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import com.mojang.blaze3d.systems.RenderSystem;
import com.mojang.blaze3d.vertex.PoseStack;

import net.minecraft.ChatFormatting;
import net.minecraft.client.Minecraft;
import net.minecraft.client.gui.components.Button;
import net.minecraft.client.gui.components.EditBox;
import net.minecraft.client.gui.screens.ConfirmScreen;
import net.minecraft.client.gui.screens.Screen;
import net.minecraft.client.gui.screens.inventory.AbstractContainerScreen;
import net.minecraft.client.renderer.GameRenderer;
import net.minecraft.network.chat.CommonComponents;
import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.network.chat.TranslatableComponent;
import net.minecraft.world.entity.player.Inventory;

import net.minecraftforge.api.distmarker.Dist;
import net.minecraftforge.api.distmarker.OnlyIn;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.client.screen.ScreenHelper;
import de.markusbordihn.easynpc.dialog.DialogType;
import de.markusbordihn.easynpc.entity.EasyNPCEntity;
import de.markusbordihn.easynpc.menu.configuration.MainConfigurationMenu;
import de.markusbordihn.easynpc.network.NetworkHandler;
import de.markusbordihn.easynpc.skin.SkinType;

@OnlyIn(Dist.CLIENT)
public class MainConfigurationScreen extends AbstractContainerScreen<MainConfigurationMenu> {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected final EasyNPCEntity entity;
  protected final UUID uuid;

  // Internal
  protected Button editActionButton = null;
  protected Button editDialogButton = null;
  protected Button editSkinButton = null;
  protected Button removeEntityButton = null;
  protected Button saveNameButton = null;
  private List<Button> skinButtons = new ArrayList<>();
  private EditBox nameBox;
  protected float xMouse;
  protected float yMouse;

  public MainConfigurationScreen(MainConfigurationMenu menu, Inventory inventory,
      Component component) {
    super(menu, inventory, component);
    this.entity = menu.getEntity();
    this.uuid = this.entity.getUUID();
  }

  private void saveName() {
    String value = this.nameBox.getValue();
    if (value != null && !value.isBlank()) {
      log.debug("Saving name {} for {}", value, this.entity);
      NetworkHandler.nameChange(this.uuid, value);
    }
  }

  public void deleteNPC() {
    Minecraft minecraft = this.minecraft;
    if (minecraft == null) {
      return;
    }
    minecraft.setScreen(new ConfirmScreen(confirmed -> {
      if (confirmed && uuid != null) {
        NetworkHandler.removeNPC(uuid);
        minecraft.setScreen((Screen) null);
      } else {
        minecraft.setScreen(this);
      }
    }, new TranslatableComponent(Constants.TEXT_PREFIX + "removeNPC.deleteQuestion"),
        new TranslatableComponent(Constants.TEXT_PREFIX + "removeNPC.deleteWarning",
            this.entity.getDisplayName().getString()),
        new TranslatableComponent(Constants.TEXT_PREFIX + "removeNPC.deleteButton"),
        CommonComponents.GUI_CANCEL));
  }

  @Override
  public void init() {
    if (this.entity == null) {
      return;
    }
    super.init();

    // Default stats
    this.imageHeight = 220;
    this.imageWidth = 285;

    // Basic Position
    this.titleLabelX = 60;
    this.titleLabelY = 6;
    this.topPos = (this.height - this.imageHeight) / 2;
    this.leftPos = (this.width - this.imageWidth) / 2;

    // Name
    this.nameBox = new EditBox(this.font, this.leftPos + 7, this.topPos + 31, 190, 18,
        new TranslatableComponent("Name"));
    this.nameBox.setMaxLength(32);
    this.nameBox.setValue(this.entity.getName().getString());
    this.addRenderableWidget(this.nameBox);

    this.saveNameButton = this.addRenderableWidget(new Button(this.leftPos + 199, this.topPos + 30,
        80, 20, new TranslatableComponent("Save"), onPress -> {
          this.saveName();
        }));

    // Dialog
    this.editDialogButton = this.addRenderableWidget(new Button(this.leftPos + 110,
        this.topPos + 54, 83, 20, new TranslatableComponent("Dialog"), onPress -> {
          DialogType dialogType = this.entity.getDialogType();
          switch (dialogType) {
            case BASIC:
              NetworkHandler.openDialog(uuid, "BasicDialogConfiguration");
              break;
            case YES_NO:
              NetworkHandler.openDialog(uuid, "YesNoDialogConfiguration");
              break;
            default:
              NetworkHandler.openDialog(uuid, "BasicDialogConfiguration");
          }
        }));

    // Skins
    this.editSkinButton = this.addRenderableWidget(new Button(this.leftPos + 7, this.topPos + 190,
        100, 20, new TranslatableComponent("Skin"), onPress -> {
          SkinType skinType = this.entity.getSkinType();
          switch (skinType) {
            case PLAYER_SKIN:
            case SECURE_REMOTE_URL:
            case INSECURE_REMOTE_URL:
              NetworkHandler.openDialog(uuid, "PlayerSkinConfiguration");
              break;
            case CUSTOM:
              NetworkHandler.openDialog(uuid, "CustomSkinConfiguration");
              break;
            default:
              NetworkHandler.openDialog(uuid, "DefaultSkinConfiguration");
          }
        }));

    // Actions
    this.editActionButton = this.addRenderableWidget(new Button(this.leftPos + 197,
        this.topPos + 54, 83, 20, new TranslatableComponent("Actions"), onPress -> {
          NetworkHandler.openDialog(uuid, "BasicActionConfiguration");
        }));

    // Delete
    this.removeEntityButton =
        this.addRenderableWidget(new Button(this.leftPos + 229, this.topPos + 190, 50, 20,
            new TranslatableComponent("Delete").withStyle(ChatFormatting.RED), onPress -> {
              deleteNPC();
            }));
  }

  @Override
  public void render(PoseStack poseStack, int x, int y, float partialTicks) {
    this.renderBackground(poseStack);
    super.render(poseStack, x, y, partialTicks);
    this.xMouse = x;
    this.yMouse = y;

    // Name
    this.font.draw(poseStack, new TextComponent("Name"), this.leftPos + 7f, this.topPos + 20f,
        4210752);

    // Render current Avatar
    ScreenHelper.renderEntityAvatar(this.leftPos + 55, this.topPos + 185, 55,
        this.leftPos + 50 - this.xMouse, this.topPos + 90 - this.yMouse, this.entity);
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
    this.blit(poseStack, leftPos + 243, topPos, 205, 0, 45, 170);

    int expandedHeight = 50;
    this.blit(poseStack, leftPos, topPos + expandedHeight + 5, 0, 5, 250, 170);
    this.blit(poseStack, leftPos + 243, topPos + expandedHeight + 5, 205, 5, 45, 170);

    // Entity
    fill(poseStack, this.leftPos + 7, this.topPos + 54, this.leftPos + 107, this.topPos + 191,
        0xff000000);
    fill(poseStack, this.leftPos + 8, this.topPos + 55, this.leftPos + 106, this.topPos + 190,
        0xffaaaaaa);
  }

  @Override
  public boolean keyPressed(int keyCode, int unused1, int unused2) {
    if (keyCode != 257 && keyCode != 335 && keyCode != 69) {
      return super.keyPressed(keyCode, unused1, unused2);
    } else if (keyCode == 257 || keyCode == 335) {
      return true;
    } else {
      return true;
    }
  }

  @Override
  public boolean mouseClicked(double mouseX, double mouseY, int button) {
    // Make sure we pass the mouse click to the dynamically added buttons, if any.
    if (!skinButtons.isEmpty()) {
      for (Button skinButton : skinButtons) {
        skinButton.mouseClicked(mouseX, mouseY, button);
      }
    }
    return super.mouseClicked(mouseX, mouseY, button);
  }

}
