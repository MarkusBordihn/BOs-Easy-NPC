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
import de.markusbordihn.easynpc.configui.network.message.server.ChangeSoundMessage;
import de.markusbordihn.easynpc.data.sound.SoundDataEntry;
import de.markusbordihn.easynpc.data.sound.SoundType;
import java.util.UUID;

public interface ServerSoundNetworkMessageHandlerInterface {

  default void soundChange(
      UUID uuid,
      SoundType soundType,
      String soundName,
      float volume,
      float pitch,
      boolean enabled) {
    if (uuid != null && soundType != null) {
      NetworkHandlerManager.sendMessageToServer(
          new ChangeSoundMessage(
              uuid, soundType, soundName == null ? "" : soundName, volume, pitch, enabled));
    }
  }

  default void soundReset(UUID uuid, SoundType soundType) {
    this.soundChange(
        uuid,
        soundType,
        "",
        SoundDataEntry.DEFAULT_VOLUME,
        SoundDataEntry.DEFAULT_PITCH,
        SoundDataEntry.DEFAULT_ENABLED);
  }
}
