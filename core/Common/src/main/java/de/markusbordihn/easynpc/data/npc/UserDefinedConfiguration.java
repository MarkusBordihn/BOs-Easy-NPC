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

package de.markusbordihn.easynpc.data.npc;

import de.markusbordihn.easynpc.Constants;
import net.minecraft.core.registries.BuiltInRegistries;
import net.minecraft.resources.ResourceLocation;
import net.minecraft.world.entity.EntityType;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public record UserDefinedConfiguration(
    String id,
    String name,
    EntityType<?> baseEntityType,
    float width,
    float height,
    String description) {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);
  private static final float DEFAULT_WIDTH = 0.6F;
  private static final float DEFAULT_HEIGHT = 1.95F;

  public UserDefinedConfiguration(
      String id, String name, EntityType<?> baseEntityType, String description) {
    this(id, name, baseEntityType, DEFAULT_WIDTH, DEFAULT_HEIGHT, description);
  }

  public String getBaseEntityTypeId() {
    ResourceLocation resourceLocation = BuiltInRegistries.ENTITY_TYPE.getKey(baseEntityType);
    return resourceLocation.toString();
  }

  public boolean isValid() {
    return id != null
        && !id.trim().isEmpty()
        && name != null
        && !name.trim().isEmpty()
        && baseEntityType != null
        && width > 0
        && height > 0;
  }
}
