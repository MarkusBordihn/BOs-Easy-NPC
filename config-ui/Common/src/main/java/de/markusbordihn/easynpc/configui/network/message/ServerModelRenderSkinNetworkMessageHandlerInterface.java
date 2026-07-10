/*
 * Copyright 2026 Markus Bordihn
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

package de.markusbordihn.easynpc.configui.network.message;

import de.markusbordihn.easynpc.configui.network.NetworkHandlerManager;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelAnimationDataMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelEquipmentVisibilityMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelPositionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelRotationMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelScaleMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeModelVisibilityMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeNamedPoseMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangePoseMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangePositionMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeRendererMessage;
import de.markusbordihn.easynpc.configui.network.message.server.ChangeSkinMessage;
import de.markusbordihn.easynpc.data.model.ModelAnimationBehavior;
import de.markusbordihn.easynpc.data.model.ModelAnimationData;
import de.markusbordihn.easynpc.data.model.ModelPartType;
import de.markusbordihn.easynpc.data.position.CustomPosition;
import de.markusbordihn.easynpc.data.render.RenderType;
import de.markusbordihn.easynpc.data.rotation.CustomRotation;
import de.markusbordihn.easynpc.data.scale.CustomScale;
import de.markusbordihn.easynpc.data.skin.SkinDataEntry;
import java.util.Optional;
import java.util.UUID;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityType;
import net.minecraft.world.entity.EquipmentSlot;
import net.minecraft.world.entity.Pose;
import net.minecraft.world.phys.Vec3;

public interface ServerModelRenderSkinNetworkMessageHandlerInterface {

  default void setSkin(final UUID uuid, final SkinDataEntry skinDataEntry) {
    if (uuid != null && skinDataEntry != null) {
      NetworkHandlerManager.sendMessageToServer(new ChangeSkinMessage(uuid, skinDataEntry));
    }
  }

  default void poseChange(UUID uuid, Pose pose) {
    if (uuid != null && pose != null) {
      NetworkHandlerManager.sendMessageToServer(new ChangePoseMessage(uuid, pose));
    }
  }

  default void namedPoseChange(UUID uuid, ResourceLocation poseId) {
    if (uuid != null && poseId != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeNamedPoseMessage(uuid, poseId.toString()));
    }
  }

  default void positionChange(UUID uuid, Vec3 pos) {
    if (uuid != null && pos != null) {
      NetworkHandlerManager.sendMessageToServer(new ChangePositionMessage(uuid, pos));
    }
  }

  default void modelPositionChange(
      UUID uuid, ModelPartType modelPartType, CustomPosition position) {
    if (uuid != null && modelPartType != null && position != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeModelPositionMessage(uuid, modelPartType, position));
    }
  }

  default void modelAnimationBehaviorChange(UUID uuid, ModelAnimationBehavior animationBehavior) {
    if (uuid != null && animationBehavior != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeModelAnimationDataMessage(uuid, new ModelAnimationData(animationBehavior)));
    }
  }

  default void modelRotationChange(
      UUID uuid, ModelPartType modelPartType, CustomRotation rotation) {
    if (uuid != null && modelPartType != null && rotation != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeModelRotationMessage(uuid, modelPartType, rotation));
    }
  }

  default void modelScaleChange(UUID uuid, ModelPartType modelPartType, CustomScale scale) {
    if (uuid != null && modelPartType != null && scale != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeModelScaleMessage(uuid, modelPartType, scale));
    }
  }

  default void modelVisibilityChange(UUID uuid, EquipmentSlot equipmentSlot, boolean visible) {
    if (uuid != null && equipmentSlot != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeModelEquipmentVisibilityMessage(uuid, equipmentSlot, visible));
    }
  }

  default void modelVisibilityChange(UUID uuid, ModelPartType modelPartType, boolean visible) {
    if (uuid != null && modelPartType != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeModelVisibilityMessage(uuid, modelPartType, visible));
    }
  }

  default void setRenderType(UUID uuid, RenderType renderType) {
    if (uuid != null && renderType != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeRendererMessage(uuid, renderType, Optional.empty(), Optional.empty()));
    }
  }

  default void setRenderEntityType(UUID uuid, EntityType<?> entityType) {
    if (uuid != null && entityType != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeRendererMessage(
              uuid, RenderType.CUSTOM, Optional.of(entityType), Optional.empty()));
    }
  }

  default void setRenderEntityModel(UUID uuid, String renderEntityModel) {
    if (uuid != null && renderEntityModel != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeRendererMessage(
              uuid, RenderType.COBBLEMON_ENTITY, Optional.empty(), Optional.of(renderEntityModel)));
    }
  }
}
