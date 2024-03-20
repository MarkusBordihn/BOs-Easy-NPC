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

package de.markusbordihn.easynpc.entity.easynpc.data;

import de.markusbordihn.easynpc.entity.easynpc.EasyNPC;
import de.markusbordihn.easynpc.menu.MenuManager;
import de.markusbordihn.easynpc.menu.configuration.ConfigurationType;
import net.minecraft.server.level.ServerPlayer;
import net.minecraft.world.entity.LivingEntity;

public interface ConfigurationData<T extends LivingEntity> extends EasyNPC<T> {

  default void openMainConfigurationMenu(ServerPlayer serverPlayer) {
    MenuManager.getMenuHandler()
        .openConfigurationMenu(ConfigurationType.MAIN, serverPlayer, this, 0);
  }

  default boolean supportsConfiguration() {
    return true;
  }

  default boolean supportsPoseConfiguration() {
    return true;
  }

  default boolean supportsStandardPoseConfiguration() {
    return true;
  }

  default boolean supportsAdvancedPoseConfiguration() {
    return true;
  }

  default boolean supportsCustomPoseConfiguration() {
    return true;
  }

  default boolean supportsScalingConfiguration() {
    return true;
  }

  default boolean supportsSkinConfiguration() {
    return true;
  }

  default boolean supportsNoneSkinConfiguration() {
    return true;
  }

  default boolean supportsDefaultSkinConfiguration() {
    return true;
  }

  default boolean supportsUrlSkinConfiguration() {
    return true;
  }

  default boolean supportsPlayerSkinConfiguration() {
    return false;
  }

  default boolean supportsCustomSkinConfiguration() {
    return true;
  }
}
