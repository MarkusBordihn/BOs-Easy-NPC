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

package de.markusbordihn.easynpc.entity;

import java.util.UUID;
import javax.annotation.Nullable;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import net.minecraft.network.chat.Component;
import net.minecraft.network.chat.TextComponent;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.MenuProvider;
import net.minecraft.world.entity.player.Inventory;
import net.minecraft.world.entity.player.Player;
import net.minecraft.world.inventory.AbstractContainerMenu;

import net.minecraftforge.network.NetworkHooks;

import de.markusbordihn.easynpc.Constants;
import de.markusbordihn.easynpc.menu.DialogMenu;
import de.markusbordihn.easynpc.menu.configuration.action.BasicActionConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.dialog.BasicDialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.dialog.YesNoDialogConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.equipment.EquipmentConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.main.MainConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.pose.CustomPoseConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.pose.DefaultPoseConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.scaling.ScalingConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.skin.CustomSkinConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.skin.DefaultSkinConfigurationMenu;
import de.markusbordihn.easynpc.menu.configuration.skin.PlayerSkinConfigurationMenu;

public class EasyNPCEntityMenu {

  protected static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  protected EasyNPCEntityMenu() {}

  public static void openEquipmentConfigurationMenu(ServerPlayer serverPlayer,
      EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Equipment for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new EquipmentConfigurationMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openBasicActionConfigurationMenu(ServerPlayer serverPlayer,
      EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Basic Actions for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new BasicActionConfigurationMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openBasicDialogConfigurationMenu(ServerPlayer serverPlayer,
      EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Basic Dialog for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new BasicDialogConfigurationMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openYesNoDialogConfigurationMenu(ServerPlayer serverPlayer,
      EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Yes/No Dialog for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new YesNoDialogConfigurationMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openMainConfigurationMenu(ServerPlayer serverPlayer, EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Easy NPC (Non Player Character)");
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new MainConfigurationMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openCustomPoseConfigurationMenu(ServerPlayer serverPlayer,
      EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Custom Pose for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new CustomPoseConfigurationMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openDefaultPoseConfigurationMenu(ServerPlayer serverPlayer,
      EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Pose for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new DefaultPoseConfigurationMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openCustomSkinConfigurationMenu(ServerPlayer serverPlayer,
      EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Custom Skin for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new CustomSkinConfigurationMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openDefaultSkinConfigurationMenu(ServerPlayer serverPlayer,
      EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Default Skin for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new DefaultSkinConfigurationMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openPlayerSkinConfigurationMenu(ServerPlayer serverPlayer,
      EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Player Skin for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new PlayerSkinConfigurationMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openScalingConfigurationMenu(ServerPlayer serverPlayer, EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return new TextComponent("Scaling for " + entity.getName().getString());
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new ScalingConfigurationMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

  public static void openDialogMenu(ServerPlayer serverPlayer, EasyNPCEntity entity) {
    UUID uuid = entity.getUUID();
    MenuProvider provider = new MenuProvider() {
      @Override
      public Component getDisplayName() {
        return entity.getName();
      }

      @Nullable
      @Override
      public AbstractContainerMenu createMenu(int windowId, Inventory inventory,
          Player serverPlayer) {
        return new DialogMenu(windowId, inventory, uuid);
      }
    };
    NetworkHooks.openGui(serverPlayer, provider, buffer -> buffer.writeUUID(uuid));
  }

}
