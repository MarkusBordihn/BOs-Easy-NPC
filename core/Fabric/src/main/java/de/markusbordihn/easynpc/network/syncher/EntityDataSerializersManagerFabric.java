/*
 * Copyright 2023 Markus Bordihn
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software
 * and associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING
 * BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

package de.markusbordihn.easynpc.network.syncher;

import de.markusbordihn.easynpc.Constants;
import java.util.Map;
import net.fabricmc.fabric.api.object.builder.v1.entity.FabricTrackedDataRegistry;
import net.minecraft.network.syncher.EntityDataSerializer;
import net.minecraft.resources.Identifier;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

public class EntityDataSerializersManagerFabric {

  private static final Logger log = LogManager.getLogger(Constants.LOG_NAME);

  private EntityDataSerializersManagerFabric() {}

  public static void register() {
    for (Map.Entry<String, EntityDataSerializer<?>> entry :
        EntityDataSerializersManager.ENTITY_DATA_SERIALIZERS.entrySet()) {
      EntityDataSerializer<?> serializer = entry.getValue();
      if (serializer == null) {
        log.error("Failed to register entity data serializer {}", entry.getKey());
        continue;
      }

      try {
        Identifier id =
            Identifier.fromNamespaceAndPath(Constants.MOD_ID, entry.getKey().toLowerCase());
        FabricTrackedDataRegistry.register(id, serializer);
        log.info(
            "Registered entity data serializer {} with id {} using FabricTrackedDataRegistry",
            entry.getKey(),
            id);
      } catch (Exception e) {
        log.error(
            "Failed to register entity data serializer {}: {}", entry.getKey(), e.getMessage());
      }
    }
  }
}
